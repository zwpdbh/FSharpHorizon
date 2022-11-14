module BST

open Expecto

type Tree<'T> =
    | Empty
    | Node of value: 'T * left: Tree<'T> * right: Tree<'T>


let tree1 = Node(1, Empty, Empty)

let tree2 = Node(7, Node(3, Empty, Empty), Node(9, Empty, Empty))

let tree3 =
    Node(20, Node(10, Empty, Empty), Node(30, Node(5, Empty, Empty), Node(40, Empty, Empty)))

let tree4 = Node('a', Empty, Empty)

let tree5 = Node('b', Node('a', Empty, Empty), Node('c', Empty, Empty))

let tree6 =
    Node('c', Node('b', Empty, Empty), Node('d', Node('a', Empty, Empty), Node('e', Empty, Empty)))

let rec inOrder (tree: Tree<'T>) =
    // Inorder means the order in which the root is visited 
    seq {
        match tree with
        | Node (v, left, right) ->
            yield! inOrder left
            yield v
            yield! inOrder right
        | Empty -> yield! Seq.empty
    }


let isBST (tree: Tree<'T>) = 
    inOrder tree
    |> Seq.pairwise
    |> Seq.forall (fun (a, b) -> a <= b)


let rec insert newValue  (tree: Tree<'T>) = 
    match tree with 
    | Empty -> Node (newValue, Empty, Empty)
    | Node (v, left, right) when newValue < v -> 
        let left' = insert newValue left 
        Node (v, left', right)
    | Node (v, left, right) when newValue > v -> 
        let right' = insert newValue right 
        Node (v, left, right')
    | _ -> tree // if inserted value already exist, do nothing

let test01 =
    testCase "01 BST: in-order"
    <| fun _ ->
        let expected = [10; 20; 5; 30; 40]
        Expect.sequenceEqual expected (inOrder tree3) ""


let test02 = 
    testCase "02 BST: verification"
    <| fun _ -> 
        Expect.isTrue (isBST tree1) ""
        Expect.isTrue (isBST tree2) ""
        Expect.isFalse (isBST tree3) ""

        Expect.isTrue (isBST tree4) ""
        Expect.isTrue (isBST tree5) ""
        Expect.isFalse (isBST tree6) ""

let test03 = 
    testCase "03 BST: insert"
    <| fun _ -> 
        let tree =
            Empty 
            |> insert 2 
            |> insert 1 
            |> insert 100 
            |> insert 1 
            |> insert 4 
            |> insert 3 

        Expect.sequenceEqual (inOrder tree) (seq [1; 2; 3; 4; 100]) ""


[<Tests>]
let tests = testList "Binary Search Tree" [ test01; test02; test03 ]
