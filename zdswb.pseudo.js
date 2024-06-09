❲defobj❳ ❲SWB❳ {
    ❲fresh❳ () {
	.❲name❳ = "<❲noname❳>"
	.❲input❳ = ❲Queue❳/❲new❳ ()
	.❲state❳ = '❲idle❳'
        .❲handler❳ = ⊥
    }

    ❲method❳ ❲input❳ () { .❲input❳ }
    ❲method❳ ❲name❳ () { .❲name❳ }
    ❲method❳ ❲state❳ () { .❲state❳ }
    ❲method❳ ❲handler❳ (❲msg❳ ❲outq❳) {	❲funcall❳ (.❲handler❳ ❲msg❳ ❲outq❳) }
}
