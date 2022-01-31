// T-501-FMAL, Spring 2021, Assignment 4

// Test cases for Problem 2

// void main() {
//   int p;
//   make_range(&p, 5, 50);
//   print_array(p, 10);
// }
let print_array_test =
  ("main", [], ["p"], Block [
    Call ("make_range", [Addr (AccVar "p"); Num 5; Num 50]);
    Call ("print_array", [Access (AccVar "p"); Num 10])
  ]);;

// void main() {
//   int p;
//   make_range(&p, 5, 10);
//   print_array(p, 6);
// }
let print_array_test2 =
  ("main", [], ["p"], Block [
    Call ("make_range", [Addr (AccVar "p"); Num 5; Num 10]);
    Call ("print_array", [Access (AccVar "p"); Num 6])
  ]);;

// void main() {
//   int p, q;
//   make_range(&p, 5, 50);
//   make_range(&q, 100, 150);
//   memcpy(p, q, 5);
//   print_array(p, 10);
// }
let memcpy_test =
  ("main", [], ["p"; "q"], Block [
    Call ("make_range", [Addr (AccVar "p"); Num 5; Num 50]);
    Call ("make_range", [Addr (AccVar "q"); Num 100; Num 150]);
    Call ("memcpy", [Access (AccVar "p"); Access (AccVar "q"); Num 5]);
    Call ("print_array", [Access (AccVar "p"); Num 10])
  ]);;

// void main() {
//   int p, q;
//   make_range(&p, 5, 50);
//   make_copy(&q, p, 10);
//   print_array(q, 10);
// }
let make_copy_test =
  ("main", [], ["p"; "q"], Block [
    Call ("make_range", [Addr (AccVar "p"); Num 5; Num 50]);
    Call ("make_copy", [Addr (AccVar "q"); Access (AccVar "p"); Num 10]);
    Call ("print_array", [Access (AccVar "q"); Num 10])
  ]);;

// > run (Prog [make_range; print_array; print_array_test]) [] |> ignore;;
// 5 6 7 8 9 10 11 12 13 14 val it : unit = ()
// > run (Prog [make_range; print_array; print_array_test2]) [] |> ignore;;
// 5 6 7 8 9 10 val it : unit = ()
// > run (Prog [make_range; memcpy; print_array; memcpy_test]) [] |> ignore;;
// 100 101 102 103 104 10 11 12 13 14 val it : unit = ()
// > run (Prog [make_range; memcpy; make_copy; print_array; make_copy_test]) [] |> ignore;;
// 5 6 7 8 9 10 11 12 13 14 val it : unit = ()



// Test cases for Problem 3

// void main() {
//   var a, b;
//   b = alloc(2);
//   *b = 10;
//   a = alloc(2);
//   *a = 11;
//   *(a + 1) = b;
//   print_list(a);
// }
let print_list_test =
  ("main", [], ["a"; "b"], Block [
    Alloc (AccVar "b", Num 2);
    Assign (AccDeref (Access (AccVar "b")), Num 10);
    Alloc (AccVar "a", Num 2);
    Assign (AccDeref (Access (AccVar "a")), Num 11);
    Assign (AccDeref (Op ("+", Access (AccVar "a"), Num 1)), Access (AccVar "b"));
    Call ("print_list", [Access (AccVar "a")]);
  ]);;

// void main() {
//   var a, b;
//   make_range(&a, 100, 105);
//   array_to_list(&b, a, 6);
//   print_list(b);
// }
let print_list_test2 =
  ("main", [], ["a"; "b"], Block [
    Call ("make_range", [Addr (AccVar "a"); Num 100; Num 105]);
    Call ("array_to_list", [Addr (AccVar "b"); Access (AccVar "a"); Num 6]);
    Call ("print_list", [Access (AccVar "b")]);
  ]);;

// > run (Prog [make_range; array_to_list; print_list; print_list_test]) [] |> ignore;;
// 11 10 val it : unit = ()
// > run (Prog [make_range; array_to_list; print_list; print_list_test2]) [] |> ignore;;
// 100 101 102 103 104 105 val it : unit = ()



// Test cases for Problem 4

// void main() {
//   int a, b;
//   a = 5;
//   test_and_set(&b, &a);
//   print a;
//   print b;
// }
let test_and_set_test =
  ("main", [], ["a"; "b"], Block [
    Assign (AccVar "a", Num 5);
    TestAndSet (Addr (AccVar "b"), Addr (AccVar "a"));
    Print (Access (AccVar "a"));
    Print (Access (AccVar "b"))
  ]);;

// void main() {
//   int p;
//   p = alloc(2);
//   *p = 3;
//   *(p + 1) = 4;
//   test_and_set(p, p + 1);
//   print(*p);
//   print(*(p + 1));
// }
let test_and_set_test2 =
  ("main", [], ["p"], Block [
    Alloc (AccVar "p", Num 2);
    Assign (AccDeref (Access (AccVar "p")), Num 3);
    Assign (AccDeref (Op ("+", Access (AccVar "p"), Num 1)), Num 4);
    TestAndSet (Access (AccVar "p"), Op ("+", Access (AccVar "p"), Num 1));
    Print (Access (AccDeref (Access (AccVar "p"))));
    Print (Access (AccDeref (Op ("+", Access (AccVar "p"), Num 1))))
  ]);;

// void main() {
//   var a, b, i;
//   make_range(&a, 55, 65);
//   make_range(&b, 105, 115);
//   while (i <= 10) {
//     test_and_set(a + i, b + i);
//     i = i + 1;
//   }
//   print_array(a, 21);
//   print_array(b, 21);
// }
let test_and_set_test3 =
  ("main", [], ["a"; "b"; "i"], Block [
    Call ("make_range", [Addr (AccVar "a"); Num 55; Num 65]);
    Call ("make_range", [Addr (AccVar "b"); Num 105; Num 115]);
    While (Op ("<=", Access (AccVar "i"), Num 10), Block [
      TestAndSet(
        Op ("+", Access (AccVar "a"), Access (AccVar "i")),
        Op ("+", Access (AccVar "b"), Access (AccVar "i")));
      Assign (AccVar "i", Op ("+", Access (AccVar ("i")), Num 1))
    ]);
    Call ("print_array", [Access (AccVar "a"); Num 11]);
    Call ("print_array", [Access (AccVar "b"); Num 11])
  ]);;

// void main() {
//   var a, i;
//   make_range(&a, 55, 65);
//   while (i <= 9) {
//     test_and_set(a + i, a + (i + 1));
//     i = i + 2;
//   }
//   print_array(a, 11);
// }
let test_and_set_test4 =
  ("main", [], ["a"; "i"], Block [
    Call ("make_range", [Addr (AccVar "a"); Num 55; Num 65]);
    While (Op ("<=", Access (AccVar "i"), Num 9), Block [
      TestAndSet(
        Op ("+", Access (AccVar "a"), Access (AccVar "i")),
        Op ("+", Access (AccVar "a"), Op ("+", Access (AccVar "i"), Num 1)));
      Assign (AccVar "i", Op ("+", Access (AccVar ("i")), Num 2))
    ]);
    Call ("print_array", [Access (AccVar "a"); Num 11])
  ]);;

// void main() {
//   var a, i;
//   make_range(&a, 55, 65);
//   while (i <= 9) {
//     test_and_set(a + i, a + (i + 1));
//     i = i + 1;
//   }
//   print_array(a, 11);
// }
let test_and_set_test5 =
  ("main", [], ["a"; "i"], Block [
    Call ("make_range", [Addr (AccVar "a"); Num 55; Num 65]);
    While (Op ("<=", Access (AccVar "i"), Num 9), Block [
      TestAndSet(
        Op ("+", Access (AccVar "a"), Access (AccVar "i")),
        Op ("+", Access (AccVar "a"), Op ("+", Access (AccVar "i"), Num 1)));
      Assign (AccVar "i", Op ("+", Access (AccVar ("i")), Num 1))
    ]);
    Call ("print_array", [Access (AccVar "a"); Num 11])
  ]);;

// > run (Prog [test_and_set_test]) [] |> ignore;;
// 1 5 val it : unit = ()
// > run (Prog [test_and_set_test2]) [] |> ignore;;
// 4 1 val it : unit = ()
// > run (Prog [make_range; print_array; test_and_set_test3]) [] |> ignore;;
// 105 106 107 108 109 110 111 112 113 114 115 1 1 1 1 1 1 1 1 1 1 1 val it : unit = ()
// > run (Prog [make_range; print_array; test_and_set_test4]) [] |> ignore;;
// 56 1 58 1 60 1 62 1 64 1 65 val it : unit = ()
// > run (Prog [make_range; print_array; test_and_set_test5]) [] |> ignore;;
// 56 57 58 59 60 61 62 63 64 65 1 val it : unit = ()


