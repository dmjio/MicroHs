MHS=../bin/gmhs
TMHS=$(MHS) -i../lib
EVAL=../bin/mhseval
.PHONY: test time clean errtest alltest cache

alltest:	test errtest

cache:
	rm -f .mhscache
	$(TMHS) -c AllOfLib

test:
	$(TMHS) Hello      && $(EVAL) > Hello.out      && diff Hello.ref Hello.out
	$(TMHS) IOTest && (echo q | $(EVAL) a bb ccc | sed 's/^[0-9]*ms/1ms/' > IOTest.out) && diff IOTest.ref IOTest.out
	$(TMHS) StringTest && $(EVAL) > StringTest.out && diff StringTest.ref StringTest.out
	$(TMHS) ListTest   && $(EVAL) > ListTest.out   && diff ListTest.ref ListTest.out
	$(TMHS) Fac        && $(EVAL) > Fac.out        && diff Fac.ref Fac.out
	$(TMHS) Misc       && $(EVAL) > Misc.out       && diff Misc.ref Misc.out
	$(TMHS) Case       && $(EVAL) > Case.out       && diff Case.ref Case.out
	$(TMHS) Arith      && $(EVAL) > Arith.out      && diff Arith.ref Arith.out
	$(TMHS) Guard      && $(EVAL) > Guard.out      && diff Guard.ref Guard.out
	$(TMHS) Newtype    && $(EVAL) > Newtype.out    && diff Newtype.ref Newtype.out
	$(TMHS) LitMatch   && $(EVAL) > LitMatch.out   && diff LitMatch.ref LitMatch.out
	$(TMHS) Word       && $(EVAL) > Word.out       && diff Word.ref Word.out
	$(TMHS) Enum       && $(EVAL) > Enum.out       && diff Enum.ref Enum.out
	$(TMHS) Foreign    && $(EVAL) > Foreign.out    && diff Foreign.ref Foreign.out
	$(TMHS) MutRec     && $(EVAL) > MutRec.out     && diff MutRec.ref MutRec.out
	$(TMHS) LocalPoly  && $(EVAL) > LocalPoly.out  && diff LocalPoly.ref LocalPoly.out
	$(TMHS) Rank2      && $(EVAL) > Rank2.out      && diff Rank2.ref Rank2.out
	$(TMHS) Catch      && $(EVAL) > Catch.out      && diff Catch.ref Catch.out
	$(TMHS) FArith     && $(EVAL) > FArith.out     && diff FArith.ref FArith.out
	$(TMHS) Infix      && $(EVAL) > Infix.out      && diff Infix.ref Infix.out
	$(TMHS) Class      && $(EVAL) > Class.out      && diff Class.ref Class.out
	$(TMHS) Eq         && $(EVAL) > Eq.out         && diff Eq.ref Eq.out
	$(TMHS) Floating   && $(EVAL) > Floating.out   && diff Floating.ref Floating.out
	$(TMHS) Default    && $(EVAL) > Default.out    && diff Default.ref Default.out
	$(TMHS) Multi      && $(EVAL) > Multi.out      && diff Multi.ref Multi.out
	$(TMHS) Exists     && $(EVAL) > Exists.out     && diff Exists.ref Exists.out
	$(TMHS) TypeEq     && $(EVAL) > TypeEq.out     && diff TypeEq.ref TypeEq.out
	$(TMHS) Sieve      && $(EVAL) > Sieve.out      && diff Sieve.ref Sieve.out
	$(TMHS) Dict       && $(EVAL) > Dict.out       && diff Dict.ref Dict.out

errtest:
	sh errtester.sh < errmsg.test

time:
	@echo Expect about 10s runtime
	$(TMHS) Nfib && time -p $(EVAL)

clean:
	rm -f *.out *.tmp
