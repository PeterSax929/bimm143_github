# Class 6: R functions
Peter Sax

- [1. Generate `add` function](#1-generate-add-function)
- [2. Generate DNA Sequence](#2-generate-dna-sequence)
- [3. Generate protein sequence](#3-generate-protein-sequence)

Let’s start writing our first silly function to add some numbers:

Every R function has 3 things:

- name (we get to pick this)
- input arguments (there are loads of these separated by a comma)
- the body (the R code that does the work)

## 1. Generate `add` function

``` r
add <- function(x, y){x + y}
```

I can just use this function

``` r
add(1,100)
```

    [1] 101

``` r
add(c(1,2,3,4), 100)
```

    [1] 101 102 103 104

We can set a default for x or y so that we can run the function with
only one argument

``` r
adddefault <- function(x, y=10){x + y}
adddefault(1)
```

    [1] 11

``` r
adddefault(1, 100)
```

    [1] 101

## 2. Generate DNA Sequence

> Q. Write a function to return a nucleotide sequence of a user
> specified length? Call it `generate_dna()`

``` r
generate_dna <- function(size){
  
  nucleotides <- c("A","T","C","G")
  sample(nucleotides, size, replace = TRUE)
}

generate_dna(5)
```

    [1] "G" "T" "G" "T" "G"

``` r
generate_dna(20)
```

     [1] "T" "A" "T" "C" "T" "C" "C" "A" "C" "T" "G" "A" "T" "T" "A" "C" "C" "G" "T"
    [20] "T"

I want the ability to return a sequence like “AGTACCTG” where the result
is only one element.

``` r
generate_dna2 <- function(size, together = TRUE){
  nucleotides <- c("A","T","C","G")
  sequence <- sample(nucleotides, size=size, replace = TRUE)
  if(together) {sequence <- paste(sequence, collapse="")}
  return(sequence)
}
```

``` r
generate_dna2(10)
```

    [1] "GGGTTGCAAA"

``` r
generate_dna2(10, together=FALSE)
```

     [1] "C" "G" "G" "C" "A" "T" "G" "C" "C" "T"

## 3. Generate protein sequence

We can get the set of 20 natural amino acids from the **bio3d** package.

``` r
#install.packages("bio3d")
```

> Q. Write a function, `generate_protein()` to return protein sequences
> of a user specified length

``` r
generate_protein1 <- function(size=5, together = TRUE){
  aminos <- c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I", "L", "K", "M", "F", "P", "S", "T","W", "Y", "V")
  protseq <- sample(aminos, size=size, replace=TRUE)
  if(together){protseq <- paste(protseq, collapse = "")}
  return(protseq)
}
```

``` r
generate_protein1(11)
```

    [1] "KNGQQVIWEEG"

> Q. Write a function that generates sequences of length 6 to 12.

``` r
sapply(6:12, generate_protein1)
```

    [1] "KPFIGI"       "WIVIIHW"      "ANAIFFSY"     "IIVRYGPGM"    "QGKHTRKINP"  
    [6] "TFKMTHPMWKQ"  "AKRPWYNIMIPN"

It would be cool and useful if I could get FASTA format output.

``` r
ans <- sapply(6:12, generate_protein1)
ans
```

    [1] "AKFPPW"       "IHHSQYS"      "NCKPTYNQ"     "KRNWGFVCL"    "HWPECMIYWN"  
    [6] "GWCPTPGGRTV"  "TTFKSVTAMMWF"

``` r
cat(paste(">ID.", 6:12,"\n", ans, sep = ""), sep="\n")
```

    >ID.6
    AKFPPW
    >ID.7
    IHHSQYS
    >ID.8
    NCKPTYNQ
    >ID.9
    KRNWGFVCL
    >ID.10
    HWPECMIYWN
    >ID.11
    GWCPTPGGRTV
    >ID.12
    TTFKSVTAMMWF

``` r
id.line <- paste(">ID.", 6:12, sep = "")
seq.line <- paste(id.line, ans, sep = "\n")
cat(seq.line, sep = "\n")
```

    >ID.6
    AKFPPW
    >ID.7
    IHHSQYS
    >ID.8
    NCKPTYNQ
    >ID.9
    KRNWGFVCL
    >ID.10
    HWPECMIYWN
    >ID.11
    GWCPTPGGRTV
    >ID.12
    TTFKSVTAMMWF

> Q. Determine if these sequences can be found in nature or if they are
> unique.

After using a BLASTp search by inputting the FASTA code, I determined
that length 9, 10, 11, 12 are unique, but 6, 7, 8 are not because there
are sequences with 100% coverage and identity.
