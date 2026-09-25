module MicroHs.FFI(makeFFI) where
import qualified Prelude(); import MHSPrelude
import Data.Char
import Data.List
import MicroHs.Desugar(LDef)
import MicroHs.Exp
import MicroHs.Expr
import MicroHs.Flags
import MicroHs.Ident
import MicroHs.Names
--import Debug.Trace

-- The export table has (internal-name, external-name, external-type)
-- Returns the C code, the header for the exports, and whether emscripten's ASYNCIFY is needed.
makeFFI :: Flags -> [(Ident, Ident, CType)] -> [IdentModule] -> [[LDef]] -> (String, String, Bool)
makeFFI _ forExps exclude dss =
  let ffiImports = nubBy eq [ (ie, n, t, mn) | ds <- dss, (_, d) <- ds, Lit (LForImp mn ie n (CType t)) <- [get d] ]
                 where get (App _ a) = a   -- if there is no IO type, we have (App primPerform (LForImp ...))
                       get a = a
                       eq (_, n, _, _) (_, n', _, _) = n == n'
      wrappers = [ t | (ImpWrapper, _, t, _) <- ffiImports]
      dynamics = [ t | (ImpDynamic, _, t, _) <- ffiImports]
      imps     = filter ((`notElem` exclude) . impModule) $ filter ((`notElem` runtimeFFI) . impName) ffiImports
      includes = nub [ inc | (ImpStatic iincs _ _, _, _, _) <- imps, inc <- iincs ]
      isJS (ImpJS _ _, _, _, _) = True
      isJS _ = False
      -- JavaScript imports are only compiled for emscripten, so other targets can still compile the code.
      jsGuard imp str = if isJS imp then "#if defined(__EMSCRIPTEN__)\n" ++ str ++ "\n#endif" else str
      jsincs   = if any isJS ffiImports then ["#if defined(__EMSCRIPTEN__)", "#include \"emscripten.h\"", "#endif"] else []
      asyncFFI = or [ sf == Interruptible | (ImpJS sf _, _, _, _) <- ffiImports ]
      mkSig (_, i, CType t) = let (as, ior) = getArrows t in mkExportSig i as ior ++ ";"
      header = unlines
        ["#include <stdint.h>",
         "#if defined(__cplusplus)",
         "extern \"C\" {",
         "#endif",
         "void mhs_init(void);",
         intercalate "\n" $ map mkSig forExps,
         "#if defined(__cplusplus)",
         "}",
         "#endif"
        ]
  in
    if not (null wrappers) || not (null dynamics) then mhsError "Unimplemented FFI feature" else
    (unlines $
      jsincs ++
      map (\ fn -> "#include \"" ++ fn ++ "\"") includes ++
      map (\ imp -> jsGuard imp (mkHdr imp)) imps ++
      ["static const struct ffi_entry imp_table[] = {"] ++
      map (\ imp -> jsGuard imp (mkEntry imp)) imps ++
      ["{ 0,0 }",
       "};",
       "const struct ffi_entry *xffi_table = imp_table;"
      ] ++
      ["static struct ffe_entry exp_table[] = {"] ++
      map mkExport forExps ++
      ["  { 0,0 }",
       "};",
       "struct ffe_entry *xffe_table = exp_table;",
       "\n"
      ] ++ zipWith mkExportWrapper [0..] forExps
    , header, asyncFFI)

mkExportSig :: Ident -> [EType] -> EType -> String
mkExportSig n as ior =
  let outT = cTypeName $ checkIO ior
      ins = zipWith (\ i a -> cTypeName a ++ " _x" ++ show i) [1::Int ..] as
   in outT ++ " " ++ unIdent n ++ "(" ++ intercalate ", " ins ++ ")"

mkExport :: (Ident, Ident, CType) -> String
mkExport (i, _, _) = "  { \"" ++ unIdent i ++ "\", 0 },"

mkExportWrapper :: Int -> (Ident, Ident, CType) -> String
mkExportWrapper no (_, n, CType t) = unlines $
  let (as, ior) = getArrows t
      r = checkIO ior
      outT = cTypeName r
      arg k a = "  mhs_from_" ++ cTypeHsName a ++ "(ffe_alloc(), 0, _x" ++ show k ++ "); ffe_apply();"
      eval = if eqEType r ior then "ffe_eval()" else "ffe_exec()"
  in  [mkExportSig n as ior ++ " {",
       "  gc_check(" ++ show (2 * length as + 4) ++ ");",
       "  ffe_push(xffe_table[" ++ show no ++ "].ffe_value);" ]
      ++ zipWith arg [1::Int ..] as ++
      if isUnit r then
        [ "  (void)" ++ eval ++ ";",
          "  ffe_pop();",
          "}"
        ]
       else
        [ "  " ++ outT ++ " _res = mhs_to_" ++ cTypeHsName r ++ "(" ++ eval ++ ", -1);",
          "  ffe_pop();",
          "  return _res;",
          "}"
        ]

impName :: (ImpEnt, String, EType, IdentModule) -> String
impName (_, s, _, _) = s

impModule :: (ImpEnt, String, EType, IdentModule) -> IdentModule
impModule (_, _, _, m) = m

mkEntry :: (ImpEnt, String, EType, IdentModule) -> String
mkEntry (ImpStatic _ IFunc  _, f, t, _) = "{ \"" ++ f ++ "\", " ++ show (arity t) ++ ", mhs_" ++ f ++ "},"
mkEntry (ImpStatic _ IPtr   _, f, _, _) = "{ \"&" ++ f ++ "\", 0, mhs_addr_" ++ f ++ "},"
mkEntry (ImpStatic _ IValue _, f, _, _) = "{ \"" ++ f ++ "\", 0, mhs_" ++ f ++ "},"
mkEntry (ImpJS _ _,            f, t, _) = "{ \"" ++ f ++ "\", " ++ show (arity t) ++ ", mhs_" ++ f ++ "},"
mkEntry _ = undefined

mkMhsFun :: String -> String -> String
mkMhsFun fn body = "from_t mhs_" ++ fn ++ "(int s) { " ++ body ++ "; }"

checkIO :: EType -> EType
checkIO iot =
  case dropApp identIO iot of
    Nothing -> iot -- errorMessage (getSLoc iot) $ "foreign return type must be IO: " ++ showEType iot
    Just t  -> t

dropApp :: Ident -> EType -> Maybe EType
dropApp i (EApp (EVar i') t) | i == i' = Just t
dropApp _ _ = Nothing

isUnit :: EType -> Bool
isUnit (EVar unit) = unit == identUnit
isUnit _ = False

mkRet :: HasCallStack => EType -> Int -> String -> String
mkRet t n call = "mhs_from_" ++ cTypeHsName t ++ "(s, " ++ show n ++ ", " ++ call ++ ")"

mkArg :: EType -> Int -> String
mkArg t i = "mhs_to_" ++ cTypeHsName t ++ "(s, " ++ show i ++ ")"

mkJSArg :: EType -> Int -> String
mkJSArg t i = "mhs_to_" ++ jsTypeName t ++ "(s, " ++ show i ++ ")"

mkHdr :: (ImpEnt, String, EType, IdentModule) -> String
mkHdr (ImpStatic _ IPtr fn, f, iot, _) =
  let r = checkIO iot
      (s, _) =
        case dropApp identPtr r of
          Just t  -> ("", t)
          Nothing ->
            case dropApp identFunPtr r of
              Just t  -> ("(HsFunPtr)", t)
              Nothing -> errorMessage (getSLoc r) "foreign & must be Ptr/FunPtr"
      body = "return " ++ mkRet r 0 (s ++ "&" ++ fn)
  in  mkMhsFun ("addr_" ++ f) body
mkHdr (ImpStatic _ IFunc fn, f, t, _) =
  let (as, ior) = getArrows t
      r = checkIO ior
      len = length as
      call = fn ++ "(" ++ intercalate ", " (zipWith mkArg as [0..]) ++ ")"
      fcall =
        if isUnit r then
          call ++ "; return mhs_from_Unit(s, " ++ show len ++ ")"
        else
          "return " ++ mkRet r len call
  in  mkMhsFun f fcall
mkHdr (ImpStatic _ IValue val, f, t, _) =
  let (as, ior) = getArrows t
      r = checkIO ior
      len = length as
      call = expand val
      expand [] = []
      expand ('$':c:cs) | isDigit c =
        let n = digitToInt c - 1
        in mkArg (as !! n) n ++ expand cs
      expand (c:cs) = c : expand cs
      fcall =
        if isUnit r then
          call ++ "; return mhs_from_Unit(s, " ++ show len ++ ")"
        else
          "return " ++ mkRet r len call
  in  mkMhsFun f fcall
mkHdr (ImpJS _ s, f, ty, _) | s == "wrapper" || s == "wrapper sync" =
  -- foreign import javascript "wrapper [sync]" mk :: (JSVal -> ... -> IO r) -> IO JSVal
  -- Creates a JavaScript function that calls the Haskell function.
  let (as, ior) = getArrows (dropForallContext ty)
      bad msg = errorMessage (getSLoc ty) $ "foreign import javascript \"" ++ s ++ "\": " ++ msg
      fty = case as of
              [t] -> t
              _   -> bad "expected one function argument"
      (fas, fior) = getArrows (dropForallContext fty)
      fr = checkIO fior
      ret = case jsKind fr of
              KUnit  -> "0"
              KJSVal -> "1"
              _      -> bad "the function must return IO () or IO JSVal"
      sync = if s == "wrapper sync" then "1" else "0"
      n = length fas
      call = "EM_ASM_INT({ return Module.mhsjs.mkCallback($0, " ++ show n ++ ", " ++ sync ++ ", " ++ ret ++ ") }, mhs_to_HsStablePtr(s, 0))"
  in  if any ((/= KJSVal) . jsKind) fas then bad "the function arguments must be JSVal" else
      if jsKind (checkIO ior) /= KJSVal then bad "the result must be IO JSVal" else
      mkMhsFun f ("return mhs_from_JSVal(s, 1, " ++ call ++ ")")
mkHdr (ImpJS sf s, f, ty, _) =
  -- The JavaScript code is compiled (once) on the JavaScript side into a function
  -- with parameters $1, $2, ..., see Module.mhsjs in eval.c.
  -- The code is passed as the last argument (a C string).
  -- With 'unsafe' a JavaScript exception is fatal, with 'safe' it is turned into
  -- a Haskell JSException.  With 'interruptible' the JavaScript function is async
  -- (so it can use await), and the Haskell program waits for the result (using ASYNCIFY).
  let (as, ior) = getArrows (dropForallContext ty)
      rt = checkIO ior
      rk = jsKind rt
      n = length as
      ixs = [0 .. n-1]
      -- Argument names: $i in EM_ASM code, ai as parameters of the EM_ASYNC_JS function
      argName i = if sf == Interruptible then 'a' : show i else '$' : show i
      srcName = argName n
      jsArgConv t i =
        case jsKind t of
          KJSVal -> "Module.mhsjs.getJSVal(" ++ argName i ++ ")"
          KBool  -> "!!" ++ argName i
          _      -> argName i
      jsargs = intercalate ", " (zipWith jsArgConv as ixs)
      callJS = if sf == Interruptible then "callAsync" else "call"
      body = (if sf == Interruptible then "await " else "") ++
             "Module.mhsjs." ++ callJS ++ "(" ++ srcName ++ ", " ++ show n ++ ", [" ++ jsargs ++ "])"
      -- JavaScript code for the result, and the C type of the result
      (jsres0, ctype, asmm) =
        case rk of
          KUnit   -> (body, "void", "")
          KJSVal  -> ("return Module.mhsjs.newJSVal(" ++ body ++ ")", "int", "_INT")
          KBool   -> ("return (" ++ body ++ ") ? 1 : 0", "int", "_INT")
          KInt    -> ("return " ++ body, "int", "_INT")
          KWord   -> ("return " ++ body, "int", "_INT")
          KDouble -> ("return " ++ body, "double", "_DOUBLE")
          KFloat  -> ("return " ++ body, "double", "_DOUBLE")
          KPtr    -> ("return " ++ body, "void *", "_PTR")
      -- With safe/interruptible any JavaScript exception (also in the argument
      -- conversion, e.g. a freed JSVal) is saved and raised as a JSException by mhs_js_check_error.
      jsres = if sf == Unsafe then jsres0 else
              "try { " ++ jsres0 ++ " } catch (e) { Module.mhsjs.error = e; Module.mhsjs.hasError = true; return 0; }"
      cargs = intercalate ", " (zipWith mkJSArg as ixs ++ [cString s])
      ret r = case rk of
                KUnit -> "return mhs_from_Unit(s, " ++ show n ++ ")"
                _     -> "return mhs_from_" ++ jsTypeName rt ++ "(s, " ++ show n ++ ", " ++ r ++ ")"
      check = if sf == Unsafe then "" else "mhs_js_check_error(); "
      jsCType t = case jsKind t of
                    KDouble -> "double"
                    KFloat  -> "double"
                    _       -> "int"
      asyncFun = "mhs_async_" ++ f
      asyncDecl = "EM_ASYNC_JS(" ++ ctype ++ ", " ++ asyncFun ++ ", (" ++
                    intercalate ", " (zipWith (\ t i -> jsCType t ++ " " ++ argName i) as ixs ++ ["const char *" ++ srcName]) ++
                  "), { " ++ jsres ++ "; });\n"
      asyncCall = asyncFun ++ "(" ++ cargs ++ ")"
      asmCall = "EM_ASM" ++ asmm ++ "({ " ++ jsres ++ " }, " ++ cargs ++ ")"
      fbody call =
        case rk of
          KUnit -> call ++ "; " ++ check ++ ret ""
          _     -> ctype ++ " r = " ++ call ++ "; " ++ check ++ ret "r"
      -- An interruptible import cannot be nested inside another asynchronous operation.
      fbodyAsync call =
        case rk of
          KUnit -> "mhs_js_async_begin(); " ++ call ++ "; mhs_js_async_end(); " ++ check ++ ret ""
          _     -> "mhs_js_async_begin(); " ++ ctype ++ " r = " ++ call ++ "; mhs_js_async_end(); " ++ check ++ ret "r"
  in  if sf == Interruptible then
        asyncDecl ++ mkMhsFun f (fbodyAsync asyncCall)
      else
        mkMhsFun f (fbody asmCall)
mkHdr _ = undefined

arity :: EType -> Int
arity = length . fst . getArrows . dropForallContext

-- Use to construct 'foreign import/export ccall' wrapper.
cTypeHsName :: HasCallStack => EType -> String
cTypeHsName (EApp (EVar ptr) _t) | ptr == identPtr = "Ptr"
                                 | ptr == identFunPtr = "FunPtr"
cTypeHsName (EVar i) | Just c <- lookup (unIdent i) cHsTypes = c
cTypeHsName t = errorMessage (getSLoc t) $ "Not a valid C type: " ++ showEType t

cHsTypes :: [(String, String)]
cHsTypes =
  [ ("Primitives.Float",  "Float")
  , ("Primitives.Double", "Double")
  , ("Primitives.Int",    "Int")
  , ("Primitives.Int64",  "Int64")
  , ("Primitives.Word",   "Word")
  , ("Primitives.Word64", "Word64")
  , ("()",                "Unit")
  , ("System.IO.Handle",  "Ptr")
  ]

-- Use to construct 'foreign export ccall' signature.
cTypeName :: EType -> String
cTypeName (EApp (EVar ptr) _t) | ptr == identPtr = "void*"
cTypeName (EVar i) | Just c <- lookup (unIdent i) cTypes = c
cTypeName t = errorMessage (getSLoc t) $ "Not a valid C type: " ++ showEType t

cTypes :: [(String, String)]
cTypes =
  [ ("Primitives.Float",  "float")
  , ("Primitives.Double", "double")
  , ("Primitives.Int",    "intptr_t")   -- value_t
  , ("Primitives.Int64",  "int64_t")
  , ("Primitives.Word",   "uintptr_t")  -- uvalue_t
  , ("Primitives.Word64", "uint64_t")
  , ("()",                "void")
  , ("System.IO.Handle",  "void*")
  ]

-- Types allowed in 'foreign import javascript'.
data JSKind = KJSVal | KBool | KInt | KWord | KDouble | KFloat | KPtr | KUnit
  deriving (Eq)

jsKind :: EType -> JSKind
jsKind (EApp (EVar ptr) _) | ptr == identPtr = KPtr
jsKind (EVar i) | Just k <- lookup (unIdent i) jsKinds = k
jsKind t = errorMessage (getSLoc t) $ "Not a valid JavaScript FFI type: " ++ showEType t

jsKinds :: [(String, JSKind)]
jsKinds =
  [ ("Primitives.JSVal",    KJSVal)
  , ("Data.Bool_Type.Bool", KBool)
  , ("Primitives.Int",      KInt)
  , ("Primitives.Word",     KWord)   -- also Char, which is a newtype of Word
  , ("Primitives.Double",   KDouble)
  , ("Primitives.Float",    KFloat)
  , ("()",                  KUnit)
  ]

-- The mhs_to_ function to use for a JavaScript argument.
jsTypeName :: EType -> String
jsTypeName t =
  case jsKind t of
    KJSVal  -> "JSVal"
    KBool   -> "Bool"
    KInt    -> "Int"
    KWord   -> "Word"
    KDouble -> "Double"
    KFloat  -> "Float"
    KPtr    -> "Ptr"
    KUnit   -> errorMessage (getSLoc t) "() is not a valid JavaScript argument type"

-- A C string literal.
cString :: String -> String
cString str = '"' : concatMap esc str ++ "\""
  where esc '"'  = "\\\""
        esc '\\' = "\\\\"
        esc c | c < ' ' || c == '\DEL' = '\\' : oct (fromEnum c)
              | otherwise = [c]
        oct n = [toEnum (fromEnum '0' + n `div` 64), toEnum (fromEnum '0' + (n `div` 8) `mod` 8), toEnum (fromEnum '0' + n `mod` 8)]

-- These are already in the runtime
runtimeFFI :: [String]
runtimeFFI = [
  "GETRAW", "GETTIMEMICRO", "GETBOOTTIMEMICRO", "acos", "add_FILE", "add_fd", "open", "add_utf8", "add_buf", "add_crlf",
  "asin", "atan", "atan2", "calloc", "closeb",
  "cos", "exp", "flushb", "fopen", "free", "getb", "getenv", "islinux", "ismacos", "iswindows", "log", "malloc",
  "md5Array", "md5BFILE", "md5String", "memcpy", "memmove", "realloc", "strlen", "strcpy",
  "putb", "sin", "sqrt", "system", "tan", "tmpname", "ungetb", "remove",
  "acosf", "asinf", "atanf", "atan2f", "cosf", "expf", "logf", "sinf", "sqrtf", "tanf",
  "scalbn", "scalbnf", "pow", "powf",
  "js_debug", "js_eval_run", "js_eval_call", "js_set_haskellCallback", "js_free_jsval", "js_take_exn", "js_exn_string",
  "readb", "writeb",
  "peekPtr", "pokePtr", "pokeWord", "peekWord",
  "add_lz77_compressor", "add_lz77_decompressor",
  "add_lzma_compressor", "add_lzma_decompressor",
  "add_rle_compressor", "add_rle_decompressor",
  "add_base64_encoder", "add_base64_decoder",
  "add_bwt_compressor", "add_bwt_decompressor",
  "peek_uint8", "poke_uint8", "peek_uint16", "poke_uint16", "peek_uint32", "poke_uint32", "peek_uint64", "poke_uint64",
  "peek_int8", "poke_int8", "peek_int16", "poke_int16", "peek_int32", "poke_int32", "peek_int64", "poke_int64",
  "peek_char", "poke_char", "peek_schar", "poke_schar", "peek_uchar", "poke_uchar",
  "peek_ushort", "poke_ushort", "peek_short", "poke_short",
  "peek_uint", "poke_uint", "peek_int", "poke_int",
  "peek_ulong", "poke_ulong", "peek_long", "poke_long",
  "peek_ullong", "poke_ullong", "peek_llong", "poke_llong",
  "peek_size_t", "poke_size_t",
  "peek_flt32", "poke_flt32",
  "peek_flt64", "poke_flt64",
  "sizeof_char", "sizeof_short", "sizeof_int", "sizeof_long", "sizeof_llong", "sizeof_size_t",
  "opendir", "closedir", "readdir", "c_d_name", "chdir", "mkdir", "getcwd",
  "getcpu",
  "get_mem", "openb_rd_mem", "openb_wr_mem",
  "new_mpz", "mpz_abs", "mpz_add", "mpz_and", "mpz_cmp", "mpz_get_d", "mpz_get_f",
  "mpz_get_si", "mpz_init_set_si", "mpz_init_set_ui",
  "mpz_ior",
  "mpz_mul", "mpz_mul_2exp", "mpz_neg", "mpz_popcount", "mpz_sub", "mpz_fdiv_q_2exp",
  "mpz_tdiv_qr", "mpz_tstbit", "mpz_xor",
  "mpz_get_si64", "mpz_init_set_si64", "mpz_init_set_ui64",
  "mpz_log2",
  "want_gmp",
  "want_imath",
  "gettimeofday",
  "EOK", "E2BIG", "EACCES", "EADDRINUSE", "EADDRNOTAVAIL", "EADV", "EAFNOSUPPORT", "EAGAIN",
  "EALREADY", "EBADF", "EBADMSG", "EBADRPC", "EBUSY", "ECHILD", "ECOMM", "ECONNABORTED",
  "ECONNREFUSED", "ECONNRESET", "EDEADLK", "EDESTADDRREQ", "EDIRTY", "EDOM", "EDQUOT",
  "EEXIST", "EFAULT", "EFBIG", "EFTYPE", "EHOSTDOWN", "EHOSTUNREACH", "EIDRM", "EILSEQ",
  "EINPROGRESS", "EINTR", "EINVAL", "EIO", "EISCONN", "EISDIR", "ELOOP", "EMFILE", "EMLINK",
  "EMSGSIZE", "EMULTIHOP", "ENAMETOOLONG", "ENETDOWN", "ENETRESET", "ENETUNREACH",
  "ENFILE", "ENOBUFS", "ENODATA", "ENODEV", "ENOENT", "ENOEXEC", "ENOLCK", "ENOLINK",
  "ENOMEM", "ENOMSG", "ENONET", "ENOPROTOOPT", "ENOSPC", "ENOSR", "ENOSTR", "ENOSYS",
  "ENOTBLK", "ENOTCONN", "ENOTDIR", "ENOTEMPTY", "ENOTSOCK", "ENOTSUP", "ENOTTY", "ENXIO",
  "EOPNOTSUPP", "EPERM", "EPFNOSUPPORT", "EPIPE", "EPROCLIM", "EPROCUNAVAIL",
  "EPROGMISMATCH", "EPROGUNAVAIL", "EPROTO", "EPROTONOSUPPORT", "EPROTOTYPE",
  "ERANGE", "EREMCHG", "EREMOTE", "EROFS", "ERPCMISMATCH", "ERREMOTE", "ESHUTDOWN",
  "ESOCKTNOSUPPORT", "ESPIPE", "ESRCH", "ESRMNT", "ESTALE", "ETIME", "ETIMEDOUT",
  "ETOOMANYREFS", "ETXTBSY", "EUSERS", "EWOULDBLOCK", "EXDEV",
  "errno",
  "strerror_r",
  "environ",
  "get_executable_path",
  "setenv", "unsetenv",
  "set_permissions", "get_permissions",
  "F_SETFL", "O_NONBLOCK", "SOL_SOCKET", "SO_DEBUG", "SO_ERROR", "SO_REUSEADDR", "SO_TYPE",
  "accept", "bind", "close", "connect", "fcntl", "getsockopt", "listen", "recv", "send", "setsockopt", "socket"
  ]

{-
-- lib/ modules that use foreign import
libImports :: [String]
libImports = [
  "Data.Integer_Type",
  "Data.Integer.Internal",
  "System.Process",
  "System.Environment",
  "System.Compress.ByteString",
  "System.Compress",
  "System.IO.TimeMilli",
  "System.IO.Open",
  "System.IO.Transducers",
  "System.IO.MD5",
  "System.IO.Base",
  "System.IO.StringHandle",
  "System.IO.Internal",
  "System.IO.Serialize",
  "System.CPUTime",
  "System.Directory",
  "System.Cmd",
  "Data.ByteString",
  "Data.Double",
  "Data.Float",
  "Foreign.Marshal.Utils",
  "Foreign.Marshal.Alloc",
  "Foreign.Storable",
  "Foreign.C.Error",
  "Primitives"
  ]
-}
