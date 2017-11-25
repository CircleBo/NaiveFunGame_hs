P := {F, Q 'diff' F}
W := {F}
while (W is not empty) do
{
    choose and remove a set A from W
    for each c in Σ do
    {
        let X be the set of states for which a transition on c leads to a state in A
        for each set Y in P for which X ∩ Y is nonempty and Y 'diff' X is nonempty do
        {
            replace Y in P by the two sets X ∩ Y and Y 'diff' X
        if Y is in W
            replace Y in W by the same two sets
        else
            if |X 'intersect' Y| <= |Y 'diff' X|
                    add X 'intersect' Y to W
            else
                    add Y 'diff' X to W
        }
    }
}
