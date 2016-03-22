package cmd

type instruction int

var (
    EXIT instruction
    PUSH1, PUSH2, PRINT instruction = instruction(1), instruction(2), instruction(3)
    NOP instruction = instruction(4)
    ADD, SUB, MUL instruction = instruction(5), instruction(6), instruction(7)
)


/* Two extra slots of buffer. */
type state struct { sp int; stk [66]int; }

func exit() {
	println("Actually exit is unsupported!");
}

func end() { println("Hurrah!"); }

func operate(op instruction, st state) state {

    if error := "Stack overflow!"; st.sp >= 64 {
        println(error); exit();
    }

    // I know, some cases are missing
    switch ; op {
        case PUSH1:
            st.stk[st.sp] = 1
            st.sp++
        case PUSH2:
            st.stk[st.sp] = 2
            st.sp++
        case ADD:
            if st.sp == 0 { break; }
            st.stk[st.sp - 1] += st.stk[st.sp]
            st.sp--
        case PRINT:
            println("[", st.sp, "] = ",
                     st.stk[st.sp])

    }

    return st
}

func exec(program [128]instruction) {

    var pc = 0
    var st state

    for pc < len(program) {
        if program[pc] == EXIT {
            return
        } else if program[pc] == NOP {
            continue
        } else {
            st = operate(program[pc], st)
        }

        pc += 1
    }

    end()
}
