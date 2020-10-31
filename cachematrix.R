# function to create a matrix that can cache its inverse

makeCacheMatrix<-function(mydata = 0) {
    
# to check if square matrix can be formed. Length of data frame should be
# perfect square for n x n matrix
  
  n_row<- sqrt(length(mydata))           
   
   if(!floor(n_row) == n_row) stop("Matrix is not square/invertible") 
    
   else n_col = n_row

      
# building square matrix from data argument 
   
   myMat<-matrix((mydata), n_row, n_col) 
   
   my_mat<-NULL 
   Inv_myMat<-NULL

      
# set() get() function definition for caching user supplied matrix 
# and inverse of it.    
   
   setmat<-function(y) my_mat<<-y
   getmat<-function() my_mat
    
   setInv<-function(x) Inv_myMat<<-x
   getInv<-function() Inv_myMat

     
# special matrix formation
   
SplMat<-list(setmat = setmat, getmat = getmat, setInv = setInv, getInv = getInv)


# this is the function call to check for inverse matrix in cache. If cache not 
# available or matrix is different from the one in cache then it creates inverse
# saves in cache and returns.     
     
   Cache_Mat<-cacheSolve(SplMat, myMat)
   
   print(Cache_Mat)
}



# Function to compute the inverse of the matrix returned by makeCacheMatrix function. 
# If the inverse has already been calculated for the same matrix, 
# then this function will return the inverse from the cache.

cacheSolve<-function(mt,mat){

# following 2 variables retrieve cache data and checks for matrix and inverse
# in cache. 
  
   cdata<-mt$getmat()
   Invm<-mt$getInv()
  
   if ((identical(cdata, mat)) && (!is.null(Invm))) return(Invm)

# if matrix is new or inverse in cache not available the below code is executed
# using solve() for inverse matrix, save in cache using setInv().    
   
   else {
  
    Invm<-solve(mat) 
    mt$setInv(Invm) 
    mt$setmat(mat)
    Invm
   
   }
}    

