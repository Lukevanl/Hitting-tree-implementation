/**
 * Translation of the diagnostics problems in diagnosis.pl to Scala using the gapt library.
 *
 * See https://www.logic.at/gapt/ and https://www.logic.at/gapt/downloads/gapt-user-manual.pdf
 *
 */
import gapt.expr.formula.fol.FOLTerm
import gapt.expr.formula.Formula
import gapt.expr.stringInterpolationForExpressions

object Diagnosis {
    // ASCII Format:
    // val and_gate_ascii = hof" !x (and(x) & -ab(x) -> (in1(x) & in2(x) <-> out(x))) "
    // Generated Unicode format:
    val and_gate = hof"∀x (and(x) ∧ ¬ab(x) → in1(x) ∧ in2(x) ↔ out(x))"

    // ASCII Format:
    // val or_gate_ascii = hof" !x (or(x) & -ab(x) -> (in1(x) | in2(x) <-> out(x))) "
    // Generated Unicode format:
    val or_gate = hof"∀x (or(x) ∧ ¬ab(x) → in1(x) ∨ in2(x) ↔ out(x))"


    /**
     * Two unconnected AND gates with two inputs. It is observed that the inputs are true and the outputs are false.
     * @return tuple consisting of
     *         1. SD:   list of formulae containing the system description
     *         2. COMP: list of terms that are the components of the system
     *         3. OBS:  list of formulae that are the observations
     */
    def problem1() : (List[Formula], List[FOLTerm], List[Formula]) = {
        val SD = List(
            and_gate,
            fof"and(a1)",
            fof"and(a2)"
        )

        val COMP = List(
            fot"a1",
            fot"a2"
        )

        val OBS = List(
            fof"in1(a1)",
            fof"in2(a1)",
            fof"-out(a1)",
            fof"in1(a2)",
            fof"in2(a2)",
            fof"-out(a2)"
        )

        return (SD, COMP, OBS)
    }

    /**
     * Example of wwo AND gates where the output of the first gate (a1) is connected to the first input (in1)
     * of the second gate (a2). It is easy to see that the observations are inconsistent with the specification.
     * @return tuple consisting of
     *         1. SD:   list of formulae containing the system description
     *         2. COMP: list of terms that are the components of the system
     *         3. OBS:  list of formulae that are the observations
     */
    def problem2() : (List[Formula], List[FOLTerm], List[Formula]) = {
        val SD = List(
            and_gate,
            fof"and(a1)",
            fof"and(a2)",
            fof"out(a1) <-> in1(a2)"
        )

        val COMP = List(
            fot"a1",
            fot"a2"
        )

        val OBS = List(
            fof"in1(a1)",
            fof"-in2(a1)",
            fof"out(a2)"
        )

        return (SD, COMP, OBS)
    }


    /**
     * Another wiring example, now with two AND gates and an OR gate.
     * @return tuple consisting of
     *         1. SD:   list of formulae containing the system description
     *         2. COMP: list of terms that are the components of the system
     *         3. OBS:  list of formulae that are the observations
     */
    def problem3() : (List[Formula], List[FOLTerm], List[Formula]) = {
        val SD = List(
            and_gate,
            or_gate,
            fof"and(a1)",
            fof"and(a2)",
            fof"or(o1)",
            fof"out(a1) <-> in1(o1)",
            fof"out(a2) <-> in2(o1)"
        )

        val COMP = List(
            fot"a1",
            fot"a2",
            fot"o1"
        )
        val OBS = List(
            fof"in1(a1)",
            fof"in2(a1)",
            fof"in1(a2)",
            fof"in2(a2)",
            fof"-out(o1)"
        )

        return (SD, COMP, OBS)
    }

}
