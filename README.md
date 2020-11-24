# CS599.RPackageTwo

As a project for CS599 Unsupervised Learning, I built this package of simple
unsupervised learning algorithms. Specifically, this package provides a simple
implementation of principal component analysis to perform dimensionality
reduction and a simple implementation of dynamic programming to perform
changepoint detection. Note, while these algorithms work, they are more limited
in options and less efficient than those algoirthms found in other, more
professional packages.

# Installation

This package can be installed from GitHub with the following R command.

```
remotes::install_github("Benjamin-Couey/CS599.RPackageTwo”)
```

# Usage

First, you need to import the installed package.

```
library("CS599.RPackageTwo")
```

After that, you can make use of the PCA and DYNPROG functions.
```
test.matrix <- as.matrix( iris[ 1:50, 1:4 ] )
PCA.fit <- PCA( test.matrix )
PCA.fit$rotation
PCA.fit$lambda
```

```
signal <- c( runif(25, min=0, max=2), runif(25, min=2, max=5) )
DYNPROG.fit <- DYNPROG( signal, 10 )
DYNPROG.fit$cost
DYNPROG.fit$change
```
