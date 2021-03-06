section "3"
(* *)

theory ConcreteSemantics3 imports Main "HOL-Eisbach.Eisbach" begin

type_synonym vname = string
datatype aexp = N int | V vname | Plus aexp aexp

type_synonym val = int
type_synonym state = "vname \<Rightarrow> val"

 
fun aval :: "aexp \<Rightarrow> state \<Rightarrow> val" where
"aval (N n) s = n" |
"aval (V x) s = s x" |
"aval (Plus a1 a2) s = aval a1 s + aval a2 s"

fun asimp_const :: "aexp \<Rightarrow> aexp" where
"asimp_const (N n) = N n" |
"asimp_const (V x) = V x" |
"asimp_const (Plus a1 a2) =
  (case (asimp_const a1, asimp_const a2) of
    (N n1, N n2) \<Rightarrow> N(n1 + n2)|
    (b1, b2) \<Rightarrow> Plus b1 b2)"

value "aval (Plus (N 3) (V x)) (\<lambda>x.2)"
value "aval (Plus (V x) (Plus (N 3) (N 1)))"

lemma "aval (asimp_const a) s = aval a s"
  apply (induction a)
  apply (auto split: aexp.split)
  done


section "3.1"

fun optimal :: "aexp \<Rightarrow> bool" where
"optimal (N n) = True" |
"optimal (V x) = True" |
"optimal (Plus (N n1) (N n2)) = False" |  
"optimal (Plus a1 a2) =
  (case (optimal a1, optimal a2) of
  (False, True) \<Rightarrow> False|
  (True, False) \<Rightarrow> False|
  (False, False) \<Rightarrow> False|
  (True, True) \<Rightarrow> True)"

value "optimal (Plus (N 3) (N 1))"
value "optimal (Plus (V x) (N 1))"

(* proof of optimal *)
lemma "optimal (asimp_const a) = True"
  apply (induction a)
  apply (auto split: aexp.split)
  done

section "3.2"

fun full_asimp :: "aexp \<Rightarrow> aexp" where 
"full_asimp (N n) = N n" |
"full_asimp (V x) = V x" |
"full_asimp (Plus a1 a2) =
  (case (full_asimp a1, full_asimp a2) of

    (N n1, Plus (N n2) (V x)) \<Rightarrow> (Plus (V x) (N (n1 + n2)))| 
    (N n1, Plus (V x) (N n2)) \<Rightarrow> (Plus (V x) (N (n1 + n2)))| 

    (Plus (V x) (N n1), (N n2)) \<Rightarrow> (Plus (V x) (N (n1 + n2)))| 
    (Plus (N n1) (V x), (N n2)) \<Rightarrow> (Plus (V x) (N (n1 + n2)))| 


    (N n1, N n2) \<Rightarrow> N(n1 + n2)|
    (b1, b2) \<Rightarrow> (Plus b1 b2))" 

value "full_asimp (Plus (N 3) (N 1))"
value "full_asimp (Plus (V x) (N 1))"
value "full_asimp (Plus (N 2) (Plus (N 1) (V x)))"
value "full_asimp (Plus (Plus (N 2) (N 3)) (Plus (V x) (N 1)))"
value "full_asimp (Plus (Plus (V x) (N 1)) (Plus (N 2) (N 3)))"
value "full_asimp (Plus (Plus (V x) (N 1)) (N 2))"


lemma "aval (full_asimp a) s = aval a s"
  apply (induction a)
  apply (auto split: aexp.split)
  done

section "3.3"

fun subst :: "vname \<Rightarrow> aexp \<Rightarrow> aexp \<Rightarrow> aexp" where
"subst x a (N n) = N n"|
"subst x a (V y) = ( if x=y then a else (V y) )"|
"subst x a (Plus p1 p2)  = Plus (subst x a p1) (subst x a p2)" 

value "subst x (N 3) (V y)" (* expect this to return (V y) *)

value "subst x (N 3) (N 5)"
value "subst x (N 3) (Plus (V x) (N 1))"
value "subst x (N 4) (Plus (Plus (N 2) (V x)) (Plus (V x) (V y)))"

lemma "aval (subst x a e) s = aval e (s(x := aval a s))"
  apply (induction e)
  apply (auto)
  done
 
end
