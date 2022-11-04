#' Weight function
#'
#' This function allows you find a within school allocation.
#' @param var1 set the default values.
#' @keywords allocation
#' @export
#' @examples
#' allocation_KP()



allocation_KP <- function(var1,var2,var3,var4,var5) {
  
  {
    set.seed(1)
    # set the samples
    v1 = var1
    v2 = var2 # common group
    v3 = var3
    # required sample for both groups
    rs_1 = var4
    rs_2 = var5
    # set the bins with one case each one.
    # p = c(p1=1,p2=1,p3=1,p4=1)
    p1 = ifelse(v1 == 0,0,1)
    p2 = ifelse(v2 == 1,sample(c(0,1),2,replace=FALSE)[1],1)
    p3 = ifelse(v2 == 1,sample(c(0,1),2,replace=FALSE)[2],1)
    p4 = ifelse(v1 == 0,0,1)
    # we remove from the cases from the samples
    i_v1 = ifelse(v1 == 0,0,v1 - 1)
    i_v2 = ifelse(v2 == 1,0,v2 - 2)
    i_v3 = ifelse(v3 == 0,0,v3 - 1)
    
    {
      c1 = c(p1 = (i_v1 == 0),
             p2 = (i_v2 == 0),
             p3 = (i_v2 == 0),
             p4 = (i_v3 == 0))
      # We gather the neccesary cases.
      c2 = c(p1 = (p1 + p2 == rs_1),
             p2 = (p1 + p2 == rs_1),
             p3 = (p3 + p4 == rs_2),
             p4 = (p3 + p4 == rs_2))
      # C3 = combined conditions 1 & 2.
      c3 = c(ifelse(c1[1] == TRUE | c2[1] == TRUE,0,1),
             ifelse(c1[2] == TRUE | c2[2] == TRUE,0,1),
             ifelse(c1[3] == TRUE | c2[3] == TRUE,0,1),
             ifelse(c1[4] == TRUE | c2[4] == TRUE,0,1))
      sw_sc = (c(v1,v2,v2,v3)/c(p1,p2,p3,p4))*c3
      sw_sc[which(is.nan(sw_sc) == TRUE)] = 0
    }
  }
  
  i = 0
  j <- 1
  w <- NULL
  while(i == FALSE){
    # sampled cases
    sc = c(p1,p2,p3,p4)
    # respective we(ights
    w_sc = c(v1,v2,v2,v3)/sc 
    w_sc[which(is.nan(w_sc) == TRUE)] = 0
    w <- rbind(w,w_sc)
    p1 = p1 + ifelse(which.max(sw_sc) == 1,1,0)
    p2 = p2 + ifelse(which.max(sw_sc) == 2,1,0)
    p3 = p3 + ifelse(which.max(sw_sc) == 3,1,0)
    p4 = p4 + ifelse(which.max(sw_sc) == 4,1,0)
    # we remove one of the cases from the total sample
    i_v1 = i_v1 - ifelse(which.max(sw_sc) == 1,1,0)
    i_v2 = i_v2 - ifelse(which.max(sw_sc) == 2 | which.max(sw_sc) == 3 ,1,0)
    i_v3 = i_v3 - ifelse(which.max(sw_sc) == 4,1,0)
    # which cases need to be considered
    # condition 1 for all the pools
    # which bin are available
    c1 = c(p1 = (i_v1 == 0),
           p2 = (i_v2 == 0),
           p3 = (i_v2 == 0),
           p4 = (i_v3 == 0))
    # We gather the neccesary cases.
    c2 = c(p1 = (p1 + p2 == rs_1),
           p2 = (p1 + p2 == rs_1),
           p3 = (p3 + p4 == rs_2),
           p4 = (p3 + p4 == rs_2))
    # C3 = combined conditions 1 & 2.
    c3 = c(ifelse(c1[1] == TRUE | c2[1] == TRUE,0,1),
           ifelse(c1[2] == TRUE | c2[2] == TRUE,0,1),
           ifelse(c1[3] == TRUE | c2[3] == TRUE,0,1),
           ifelse(c1[4] == TRUE | c2[4] == TRUE,0,1))
    sw_sc = w_sc*c3
    i = ifelse(sum(c3) == 0,TRUE,FALSE)
    # print the results
    
    print("------------")
    print(c(p1,p2,p3,p4))
    j <- j+1
    
    w <<- rbind(w,w_sc)
  }
  
  pk <<- c(p1,p2,p3,p4)
  names(pk) <<- c("p1","p2","p3","p4")
  #w_sc <<- w[j,]  
}






