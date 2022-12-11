  $ ./tree.exe
  Inserting 1 into Leaf makes Node (1, Leaf, Leaf)
  Inserting 2 into Node (1, Leaf, Leaf) makes Node (1, Leaf, Node (2, Leaf, Leaf))
  Inserting 3 into Node (1, Leaf, Node (2, Leaf, Leaf)) makes Node (1, Leaf, Node (2, Leaf, Node (3, Leaf, Leaf)))
  Inserting 4 into Node (1, Leaf, Node (2, Leaf, Node (3, Leaf, Leaf))) makes Node (1, Leaf, Node (2, Leaf, Node (3, Leaf, Node (4, Leaf, Leaf))))
  Inserting 3 into Leaf makes Node (3, Leaf, Leaf)
  Inserting 2 into Node (3, Leaf, Leaf) makes Node (3, Node (2, Leaf, Leaf), Leaf)
  Inserting 4 into Node (3, Node (2, Leaf, Leaf), Leaf) makes Node (3, Node (2, Leaf, Leaf), Node (4, Leaf, Leaf))
  Inserting 1 into Node (3, Node (2, Leaf, Leaf), Node (4, Leaf, Leaf)) makes Node (3, Node (2, Node (1, Leaf, Leaf), Leaf), Node (4, Leaf, Leaf))
  Inverse insert: 8
