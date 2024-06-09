I have a way of working around this, but, wonder if there's another way to look at this problem...

I want to use space-skipping, but, run into problems when creating DSLs that don't use commas.

For example:

original:
```
    method handler (msg outq) { funcall (.handler msg outq) }
```

OhmJS space skipping:
```
methodhandler(msgoutq){funcall(.handlermsgoutq)}
```

(where many distinct identifiers collapse into single ids, like `msg outq` -> `msgoutq`)

Suggestions?


---

Appendix: my workaround

pre-bracket all runs of alnums
```
    ❲method❳ ❲handler❳ (❲msg❳ ❲outq❳) { ❲funcall❳ (.❲handler❳ ❲msg❳ ❲outq❳) }
```
which keeps ids distinct even after space-skipping:
```
❲method❳❲handler❳(❲msg❳❲outq❳){❲funcall❳(.❲handler❳❲msg❳❲outq❳)}
```

but, this can't be done in the same grammar (I think)
