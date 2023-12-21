
### 1. 

An individual in the population is a chessboard with some placement of the n queens on it. The first task is to code an individual. You are to consider three encodings for this question.

* (a) A collection (e.g., a list—but the choice of data structure is up to you) of n pairs denoting the coordinates of each queen, e.g., (5, 6) would mean that a queen is standing in row 5 and column 6.

```{r 1.1.a}
#1.1.a 
# Pairs representing the coordinates of each queen, stored in a matrix
# Representation_type = "pairs"
n <- 5
pairs <- matrix(nrow = n, ncol = 2)
for (i in 1:n){
  pairs[i,] <- sample(1:n, 2)
}
typeof(pairs) #integer
ncol(pairs) #2
print(pairs)
```

* (b) On n numbers, where each number has log2 n binary digits—this number encodes the position of the queen in the given column. Notice that as queens cannot attack each other, in a legal configuration there can be only one queen per column. You can pad your binary representation with 0s if necessary.

```{r 1.1.b}
#1.1.b
# A list of n numbers, where each number is a binary representation of the position of the queen 
# in the given column
# Representation_type = "binary"
n <- 5
binary <- list()
for (i in 1:n){
  binary[[i]] <- c(as.integer(intToBits(sample(1:n, 1))))
}
typeof(binary) #list
ncol(binary) #NULL
print(binary)

```

*(c) On n numbers, where each number is the row number of the queen in each column. Notice that this encoding differs from the previous one by how the row position is stored. Here it is an integer, in item 1b it was represented through its binary representation. This will induce different ways of crossover and mutating the state.


```{r 1.1.c}
#1.1.c
# A list of n numbers, where each number is the row number of the queen in each column
# Representation_type = "singles"
n <- 5

row <- c()
for (i in 1:n){
  row[i] <- sample(1:n, 1)
}
typeof(row) #integer
ncol(row) #NULL
print(row)
```
The tasks below, 2–7 are to be repeated for each of the three encodings above.


### 2. 

Define the function crossover(): for two chessboard layouts it creates a kid by taking columns 1,..., p from the first individual and columns p + 1,..., n from the second. Obviously, 0 < p <= n/2, and p in N. Experiment with different values of p.

```{r 1.2}
#1.2
crossover <- function(layout1, layout2, p){
  #Given 2 chessboard layouts, returns a kid by taking columns 1,..., p from the first individual and columns p + 1,..., n from the second.
  if(typeof(layout1) == "list" & is.null(ncol(layout1))){
    #BINARY (one binary rep = one column)
    #print("Cross: Binary rep detected")
    #Idea: take the first p columns from layout 1 and adds them to the kid, then take the last n-p colmuns from layout 2 and adds them to the kid
    kid <- list()
    for (i in 1:p){
      kid[[i]] <- layout1[[i]]
    }
    for (i in (p+1):length(layout2)){
      kid[[i]] <- layout2[[i]]
    }
    
    return(kid)
  }
  else if(typeof(layout1) == "integer" & is.null(ncol(layout1))){
    #SINGLES
    #print("Cross: Singles rep detected")
    #Take the first p number from layout 1 and adds them to the kid, then take the last n-p numbers from layout 2 and adds them to the kid
    kid <- c()
    for (i in 1:p){
      kid <- c(kid , layout1[i])
    }
    for (i in (p+1):length(layout2)){
      kid <- c(kid , layout2[i])
    }
    return(kid)
  }else if(typeof(layout1) == "integer" & ncol(layout1) == 2){
    #PAIRS (matrix)
    #print("Cross: Pairs rep detected")
    #Idea : for layout1 and layout 2, take the first p pairs from layout1 and the last n-p pairs from layout2 by looping through the lists
    kid <- matrix(nrow = nrow(layout1), ncol = 2)
    for (i in 1:p){
      kid[i,] <- layout1[i,]
    }
    for (i in (p+1):nrow(layout2)){
      kid[i,] <- layout2[i,]
    }
    return(kid)
  }
  
}

```

```{r crossover.test}
#Testing the crossover function for n = 4, layout 1 is full of 1s, layout 2 is full of 0s, and p = 2

#binary rep: 
layout1 <- list()
layout2 <- list()
for (i in 1:4){
  layout1[[i]] <- c(1,1,1,1)
  layout2[[i]] <- 0
}
print("layout1")
vizualize_board(layout1, 4)
print("layout2")
vizualize_board(layout2, 4)
cross1 <- crossover(layout1, layout2, 2)
print("Crossover of layout1 and layout2 on p = 2")
vizualize_board(cross1, 4)
print("_____________________")

#singles rep:
layout3 <- as.integer(c(1,2,3,4))
layout4 <- as.integer(c(4,3,2,1))
print("layout3")
vizualize_board(layout3, 4)
print("layout4")
vizualize_board(layout4, 4)
cross2 <- crossover(layout3, layout4, 2)
print("Crossover of layout3 and layout4 on p = 2")
vizualize_board(cross2, 4)
print("_____________________")

#pairs rep:
layout5 <- matrix(as.integer(c(1,1,1,1,1,2,3,4)), nrow = 4, ncol = 2)
layout6 <- matrix(as.integer(c(4,4,4,4,1,2,3,4)), nrow = 4, ncol = 2)
print("layout5")
vizualize_board(layout5, 4)
print("layout6")
vizualize_board(layout6, 4)
cross3 <- crossover(layout5, layout6, 2)
print("Crossover of layout5 and layout6 on p = 2")
vizualize_board(cross3, 4)
```

### 3. 

Define the function mutate() that randomly moves a queen to a new position.

```{r 1.3}
#1.3
mutate <- function(layout,n){
  
  #Given a chessboard layout, returns a layout with a randomly moved queen
  
  if(typeof(layout) == "list" & is.null(ncol(layout))){
    #BINARY 
    print("Mut: Binary rep detected")
    #randomly select a 0 location
    zero_coords <- c()
    for (i in 1:n){
      for (j in 1:n){
        if(layout[[i]][j] == 0){
          zero_coords <- c(zero_coords, i)
          zero_coords <- c(zero_coords, j)
        }
      }
    }
    rand_idx0 <- sample(length(zero_coords)/2, 1)
    y0 <- zero_coords[rand_idx0*2]
    x0 <- zero_coords[rand_idx0*2 - 1]
    
    #randomly select a 1 location
    one_coords <- c()
    for (i in 1:n){
      for (j in 1:n){
        if(layout[[i]][j] == 1){
          one_coords <- c(one_coords, i)
          one_coords <- c(one_coords, j)
        }
      }
    }
    rand_idx1 <- sample(length(one_coords)/2, 1)
    y1 <- one_coords[rand_idx1*2]
    x1 <- one_coords[rand_idx1*2 - 1]
    
    #set the 0 to a 1 and the 1 to a 0
    if(!is.null(x0)){
      layout[[x0]][y0] <- 1
    }
    
    layout[[x1]][y1] <- 0
    return(layout)
    
  }else if(typeof(layout) == "integer" & is.null(ncol(layout))){
    #SINGLES
    print("Mut: Singles rep detected")
    #randomly select a row and change the value of the number at this position, and check if the new queen is not on another queen , and if it is not off the board
    row <- sample(n,1)
    layout[row] <- sample(n,1)
    #Disadvantages of this method : a queen can only be moved within a column, and it can also not be moved at all
    return(layout)
    
  }else if(typeof(layout) == "integer" & ncol(layout) == 2){
    #PAIRS (matrix)
    print("Mut: Pairs rep detected")
    #randomly select a row and a column and change the value of the pair at this position
    #check if the new queen is not on another queen , and if it is not off the board
    pair <- sample(n,1)
    layout[pair,1] <- sample(n,1)
    layout[pair,2] <- sample(n,1)
    #Disadvantages of this layout : can move to a location where there is already a queen 
    # --> Good or bad ? Good: simulate a queen "disappearing", bad: duplicates in the representation
    return(layout)
  }
}
```

```{r mutate.test}
#Testing the mutate function for each representation
#binary rep:
layout1 <- list()
layout1[[1]] <- c(0,0,0,0)
layout1[[2]] <- c(1,1,0,0)
layout1[[3]] <- c(0,0,0,0)
layout1[[4]] <- c(0,0,0,0)
vizualize_board(layout1, 4)
mutated1 <- mutate(layout1, 4)
vizualize_board(mutated1, 4)
print("_____________________________")

#singles rep:
layout2 <- as.integer(c(1,2,1,3))
vizualize_board(layout2, 4)
mutated2 <- mutate(layout2, 4)
vizualize_board(mutated2, 4)
print("_____________________________")

#pairs rep:
layout3 <- matrix(as.integer(c(1,1,1,1,1,2,3,4)), nrow = 4, ncol = 2)
vizualize_board(layout3, 4)
mutated3 <- mutate(layout3, 4)
vizualize_board(mutated3, 4)
```

### 4. 

Define a fitness function for a given configuration. 
Experiment with three: 
  1) binary — is a solution or not; "binary"
2) number of queens not attacked; "num
3) (nC2 - number) of pairs of queens attacking each other. 
If needed scale the value of the fitness function to [0, 1]. Experiment which could be the best one. 
Try each fitness function for each encoding method. 
You should not expect the binary fitness function to work well, explain why this is so.

-> The binary function does not work well because it only returns 0 if a solution has been found, and 1 otherwise.
This is very unlikely to happen, especially on the first iterations, so the algorithm will not be able to make any progress towards the solution, it will just wonder around randomly.

```{r 1.4}
#1.1.4
fitness <- function(layout, fitness_function, n){
  #Given a chessboard layout, returns the fitness of the layout
  if(typeof(layout) == "list" & is.null(ncol(layout))){
    
    #Binary
    #print("Viz: Binary rep detected")
    #Idea: create a matrix of 0s, then replace the 0s with 1s where the queens are
    board <- matrix(nrow = n, ncol = n)
    #fill the board with 0s
    for (i in 1:nrow(board)){
      for (j in 1:ncol(board)){
        board[i,j] <- 0
      }
    }
    for (i in 1:n){
      for (j in 1:length(layout[[i]])){
        if(layout[[i]][j] == 1){
          board[i,j] <- 1
        }
      }
    }
    board <- t(board) 
  }else if(typeof(layout) == "integer" & is.null(ncol(layout))){
    #print("Fit: Singles rep detected")
    #Convert to a matrix
    board <- matrix(nrow = n, ncol = n)
    #fill the board with 0s
    for (i in 1:nrow(board)){
      for (j in 1:ncol(board)){
        board[i,j] <- 0
      }
    }
    for (i in 1:n){
      board[i,layout[i]] <- 1
    }
    board <- t(board)
  }else if(typeof(layout) == "integer" & ncol(layout) == 2){
    #print("Fit: Pairs rep detected")
    #Convert to a matrix
    board <- matrix(nrow = n, ncol = n)
    #fill the board with 0s
    for (i in 1:nrow(board)){
      for (j in 1:ncol(board)){
        board[i,j] <- 0
      }
    }
    for (i in 1:nrow(layout)){
      board[layout[i,1], layout[i,2]] <- 1
    }
  }
  #================================================================================
  #At this point, the board var should contain a matrix on the state of the board, with 1s for queens and 0s for empty spaces
  #Convert the board to a list of coordinates 
  my_vec <- c()
  for(i in 1:n){
    for(j in 1:n){
      if(board[i,j]==1){
        my_vec <- c(my_vec, c(i,j))
      }
    }
  }
  if(length(my_vec) == 0){
    return(0)
  }
  points<-array( my_vec, dim=c(2,length(my_vec)/2))
  are_attacking_eachother <- function(Q1,Q2){
    #https://stackoverflow.com/questions/57239548/how-to-check-if-a-queen-is-under-attack-in-nqueens
    #"A queen can attack another if :
    #they are on the same line : X1 = X2
    #they are on the same column : Y1 = Y2
    #they are on the same diagonal : X1-Y1 = X2-Y2 or X1+Y1 = X2+Y2"
    (Q1[1] == Q2[1] | Q1[2] == Q2[2] | Q1[1]-Q1[2] == Q2[1]-Q2[2] | Q1[1]+Q1[2] == Q2[1]+Q2[2])
}
fitness <- 0
if(length(points)/2 == 1){
  if(fitness_function == "binary"){
    return(0)
  }else if(fitness_function == "num_safe"){
    return(1)
  }else{
    #number of attacking pairs
    return(0)
  }
}else if(fitness_function == "binary"){
  for(queen in 1:((length(points)/2)-1)){
    for(other_queen in (queen+1):(length(points)/2)){
      Q1 <- c(points[1,queen], points[2,queen])
      Q2 <- c(points[1,other_queen], points[2,other_queen])
      if(are_attacking_eachother(Q1, Q2)){
        fitness <- 1
      }
    } 
  }
}else if(fitness_function == "num_safe"){
  safe <- 0
  for(queen in 1:((length(points)/2)-1)){
    for(other_queen in (queen+1):(length(points)/2)){
      Q1 <- c(points[1,queen], points[2,queen])
      Q2 <- c(points[1,other_queen], points[2,other_queen])
      if(!are_attacking_eachother(Q1, Q2)){
        safe <- safe + 2
      }
    } 
  }
  #to scale to [0,1], we can divide by the max number of queens that can be safe
  fitness <- safe
}else{
  #(nC2 - number) of pairs of queens attacking each other.
  nc2 <- (n*(n-1)/2)
  num_attacking <- 0
  for(queen in 1:((length(points)/2)-1)){
    for(other_queen in (queen+1):(length(points)/2)){
      Q1 <- c(points[1,queen], points[2,queen])
      Q2 <- c(points[1,other_queen], points[2,other_queen])
      if(are_attacking_eachother(Q1, Q2)){
        fitness <- 1
      }
    } 
  }
  fitness <- nc2 - num_attacking
}
return(fitness)
}
```

```{r fitness.test}
layout1 <- list()
layout2 <- list()
layout3 <- list()
for (i in 1:4){
  layout1[[i]] <- 0
  layout2[[i]] <- 0
  layout3[[i]] <- 0
}

layout1[[1]] <- c(1,0,0,0)
layout1[[4]] <- c(0,0,0,1)

layout2[[1]] <- c(1,0,0,0)

layout3[[1]] <- c(1,0,0,0)
layout3[[3]] <- c(0,0,0,1)

vizualize_board(layout1, 4)
print("binary fitness of layout 1")
fitness(layout = layout1, fitness_function = "binary", 4)

vizualize_board(layout2, 4)
print("binary fitness of layout 2")
fitness(layout = layout2, fitness_function = "binary", 4)

vizualize_board(layout3, 4)
print("binary fitness of layout 3")
fitness(layout = layout3, fitness_function = "binary", 4)

print("______________________________________________________")

vizualize_board(layout1, 4)
print("num_safe fitness of layout 1")
fitness(layout = layout1, fitness_function = "num_safe", 4)

vizualize_board(layout2, 4)
print("num_safe fitness of layout 2")
fitness(layout = layout2, fitness_function = "num_safe", 4)

vizualize_board(layout3, 4)
print("num_safe fitness of layout 3")
fitness(layout = layout3, fitness_function = "num_safe", 4)

print("______________________________________________________")

```

### 5. 