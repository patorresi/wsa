allocation_weights = function(x,y){
{
  set.seed(1)
  set_values = as.data.frame(apply(x[,c(6,7,8)],2,function(x){as.numeric(x)}))
  values = c(0)
  # Set the samples --------------------------------
  v_i002 <<- which(!(set_values[,1] %in% values) == TRUE & 
    !(set_values[,2] %in% values) == FALSE &
    !(set_values[,3] %in% values) == FALSE)
  # ISCED 1 only
  v_i010 <<- which(!(set_values[,1] %in% values) == FALSE & 
    !(set_values[,2] %in% values) == TRUE &
    !(set_values[,3] %in% values) == FALSE)
  # ISCED 2 only 
  v_i020 <<- which(!(set_values[,1] %in% values) == FALSE & 
    !(set_values[,2] %in% values) == FALSE &
    !(set_values[,3] %in% values) == TRUE)
  # ISCED 02 & 1 
  v_i102 <<- which(!(set_values[,1] %in% values) == TRUE & 
    !(set_values[,2] %in% values) == TRUE &
    !(set_values[,3] %in% values) == FALSE)
  # ISCED 1 & 2
  v_i120 <<- which(!(set_values[,1] %in% values) == FALSE & 
    !(set_values[,2] %in% values) == TRUE &
    !(set_values[,3] %in% values) == TRUE)
  v1 <<- ifelse(length(v_i002) == 0,length(v_i010),length(v_i002))
  v2 <<- ifelse(length(v_i102) == 0,length(v_i120),length(v_i102))
  v3 <<- ifelse(length(v_i020) == 0,length(v_i010),length(v_i020))
  # these values are relevant in the first case.
  rs_1 <<- ifelse(length(v_i002) == 0,20,8)
  rs_2 <<- 20
  # set the bins with one case each one.
  # p = c(p1=1,p2=1,p3=1,p4=1)
  p1 = ifelse(v1 == 0,0,1)
  p2 = ifelse(v2 == 0,0,ifelse(v2 == 1,sample(c(0,1),2,replace=FALSE)[1],1))
  p3 = ifelse(v2 == 0,0,ifelse(v2 == 1,sample(c(0,1),2,replace=FALSE)[1],1))
  p4 = ifelse(v1 == 0,0,1)
  # we remove from the cases from the samples
  i_v1 = ifelse(v1 == 0,0,v1[[1]] - 1)
  i_v2 = ifelse(v2 <= 1,0,v2[[1]] - 2)
  i_v3 = ifelse(v3 == 0,0,v3[[1]] - 1)
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
      c3 = c(ifelse(c1[[1]] == TRUE | c2[[1]] == TRUE,0,1),
             ifelse(c1[[2]] == TRUE | c2[[2]] == TRUE,0,1),
             ifelse(c1[[3]] == TRUE | c2[[3]] == TRUE,0,1),
             ifelse(c1[[4]] == TRUE | c2[[4]] == TRUE,0,1))
      sw_sc = (c(v1,v2,v2,v3)/c(p1,p2,p3,p4))*c3
      sw_sc[which(is.nan(sw_sc) == TRUE)] = 0
       }
}

i = 0
while(i == FALSE){
    # sampled cases
    sc = c(p1,p2,p3,p4)
    # respective weights
    w_sc = c(v1,v2,v2,v3)/sc 
    w_sc[which(is.nan(w_sc) == TRUE)] = 0
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

}

{
# The seed was set based on the code of the school.
set.seed(as.numeric(y))
  sample1 = if(length(v_i002) == 0){
    sample(v_i010,p1,replace=FALSE)}else{
    sample(v_i002,p1,replace=FALSE)}
  sample2 = if(length(v_i102) == 0){
    sample(v_i120,p2,replace=FALSE)}else{
    sample(v_i102,p2,replace=FALSE)}
  sample3 = if(length(v_i102) == 0){
    sample(v_i120,p3,replace=FALSE)}else{
    sample(v_i102,p3,replace=FALSE)}
  sample4 = if(length(v_i020) == 0){
    sample(v_i010,p4,replace=FALSE)}else{
    sample(v_i020,p4,replace=FALSE)}
}
  samples = list(c(sample1,sample2),c(sample3,sample4))

################################################################################
# Create output file for ISCED 02
################################################################################

# After the algorithm work. we create three files. Two will be used by WinW3S
# and the remainning one will be deliver to the IEA.
# After the draft is ready, now the ouput will be assemble. 

if(length(v_i002) != 0){

# First we select the sample selected in the previous step.
df_a = x[samples[[1]],]

# create an empty matrix with 8 rows for each one of the things will add them 
# to the listing form. 
# The listing form from ISCED 02 and ISCED 1&2 Have the same number of columns
# but different number of rows (School coordinator require another row in the 
# info section)
is02_header = as.data.frame(matrix(rep("",56), nrow = 8, ncol = 7))
# the second parte is related to the additional info in top of the listing form
# ISCED 02 will not have the name of the school coordinator.
# text_title_i02 = 'TALIS 2024 - Starting Strong Survey FT - [ISCED Level 02] Listing Form'
text_title_i02 = 'TALIS 2024 - Starting Strong Survey FT - [ISCED Level 02] Listing Form'

is02_header[1,1] =  text_title_i02
is02_header[3:5,1] = c("Country/Region",'ECEC Setting Name','ECEC Setting ID')
is02_header[3:5,3] = db_info[1:3,]
# columns names in the listing form.
is02_header[8,] = c('Name',
                  'Sequence Number',
                  'Sequence Number',
                  'Leader Role',
                  'Staff Role',
                  'Year of Birth',
                  'Gender')
# reorganize the order of the columns.
df_a_filtered = df_a[,c(1,2,3,9,10,4,5)]
names(df_i1_filtered) = names(is02_header)[1:7]
is02_file = rbind(is02_header,df_i1_filtered)
# add the ending rows
# 8 rows with the header values + number of teacher/staff added to the file
# +1 blank space with the end line string
text_end_i02 = '??? Leader Role: 1 = Leader of this ECEC setting
??? Staff Role: 1 = <Only leader (no pedagogical work)>; 2 = <Teacher>; 3 = <Assistant>; 4 = <Staff for individual children>; 
5 = <Staff for special tasks>; 6 = <Intern>;  7 = <country-specific>; 8 = <country-specific>; 9 = <country-specific>; 10 = <country-specific>;   
11 = <country-specific>; 12 = <country-specific>
  ??? Year of Birth: YYYY;  9999 = Not specified
??? Gender: 1 = Female;  2 = Male; 3 = Non-binary/diverse;  9 = Refused'

is02_file[8 + nrow(df_i1_filtered)+1,] = c("","","","","","",'<list_end>')
# +2 the additional information
is02_file[8 + nrow(df_i1_filtered)+2,] = c(text_end_i02,"","","","","","")
}else{
  df_a = x[samples[[1]],]
  isa_header = as.data.frame(matrix(rep("",63), nrow = 9, ncol = 7))
}

################################################################################
# Create output file for ISCED 1
################################################################################

if(length(v_i020) != 0){
# After the algorithm work. we create three files. Two will be used by WinW3S
# and the remainning one will be deliver to the IEA.
# After the draft is ready, now the ouput will be assemble. 

df_b = x[samples[[2]],]

# create an empty matrix with 9 rows for each one of the things will add to the
# listing form. 
# As was mentioned before, this one has 9 row because additional to the things
# added for the ISCED 02 version, ISCED 1&2 have a school coordinator added.
isb_header = as.data.frame(matrix(rep("",63), nrow = 9, ncol = 7))


# the second parte is related to the additional info in top of the listing form
# ISCED 02 will not have the name of the school coordinator.
# text_title_i02 = 'TALIS 2024 - Starting Strong Survey FT - [ISCED Level 02] Listing Form'
text_title_ib  = 'TALIS 2024 FT - [ISCED Level 1&2] Teacher Listing Form'
isb_header[1,1] =  text_title_ib
isb_header[3:6,1] = c("TALIS Country/Region",'School Name','School ID','School Coordinator')
isb_header[3:6,3] = db_info
# columns names in the listing form.
isb_header[9,] = c('Teacher Name',
                  'Sequence Number',
                  'Sequence Number',
                  'Exemption',
                  'Year of Birth',
                  'Gender',
                  'Main Subject Domain at [ISCED Level 1]')
# Add the filtered teachers
# First, we need to create a empty row with the hide column
df_b$Exemption = ""
df_b_filtered = df_i2[,c(1,2,3,13,4,5,11)]
names(df_b_filtered) = names(isb_header)
df_ib_file = rbind(isb_header,df_b_filtered)
# add the ending rows
# 8 rows with the header values + number of teacher/staff added to the file
# +1 blank space with the end line string
# text_end_i02 = '??? Leader Role: 1 = Leader of this ECEC setting
# ??? Staff Role: 1 = <Only leader (no pedagogical work)>; 2 = <Teacher>; 3 = <Assistant>; 4 = <Staff for individual children>; 
# 5 = <Staff for special tasks>; 6 = <Intern>;  7 = <country-specific>; 8 = <country-specific>; 9 = <country-specific>; 10 = <country-specific>;   
# 11 = <country-specific>; 12 = <country-specific>
# ??? Year of Birth: YYYY;  9999 = Not specified
# ??? Gender: 1 = Female;  2 = Male; 3 = Non-binary/diverse;  9 = Refused'

df_ib_file[8 + nrow(df_i2_filtered)+1,] = c("","","","","","",'<list_end>')
# +2 the additional information
# df_ib_file[8 + nrow(df_i2_filtered)+2,] = c(text_end_i02,"","","","","","")
}else{
  # ISCED 1
}

################################################################################
# Create output file for ISCED 1
################################################################################



# iea file
if(length(v_i020) != 0){
# After the algorithm work. we create three files. Two will be used by WinW3S
# and the remainning one will be deliver to the IEA.
# After the draft is ready, now the ouput will be assemble. 

df_i2 = x
df_i2[,1] = "ID"
# create an empty matrix with 8 rows for each one of the things will add to the
# listing form. 
# The listing form from ISCED 02 and ISCED 1&2 Have the same number of columns
# but different number of rows (School coordinator require another row in the 
# info section)
i20_header = as.data.frame(matrix(rep("",56), nrow = 8, ncol = 7))
# the second parte is related to the additional info in top of the listing form
# ISCED 02 will not have the name of the school coordinator.
# text_title_i02 = 'TALIS 2024 - Starting Strong Survey FT - [ISCED Level 02] Listing Form'
text_title_i02 = 'TALIS 2024 - Starting Strong Survey FT - [ISCED Level 02] Listing Form'
i20_header[1,1] =  text_title_i02
i20_header[3:5,1] = c("Country/Region",'ECEC Setting Name','ECEC Setting ID')
i20_header[3:5,3] = db_info[1:3,]
# columns names in the listing form.
i20_header[8,] = c('Name',
                  'Sequence Number',
                  'Sequence Number',
                  'Leader Role',
                  'Staff Role',
                  'Year of Birth',
                  'Gender')
# Add the filtered teachers
df_i2_filtered = df_i2[,c(1,2,3,9,10,4,5)]
names(df_i2_filtered) = names(i20_header)[1:7]
i20_iea_file = rbind(i20_header,df_i2_filtered)
# add the ending rows
# 8 rows with the header values + number of teacher/staff added to the file
# +1 blank space with the end line string
text_end_i02 = '??? Leader Role: 1 = Leader of this ECEC setting
??? Staff Role: 1 = <Only leader (no pedagogical work)>; 2 = <Teacher>; 3 = <Assistant>; 4 = <Staff for individual children>; 
5 = <Staff for special tasks>; 6 = <Intern>;  7 = <country-specific>; 8 = <country-specific>; 9 = <country-specific>; 10 = <country-specific>;   
11 = <country-specific>; 12 = <country-specific>
  ??? Year of Birth: YYYY;  9999 = Not specified
??? Gender: 1 = Female;  2 = Male; 3 = Non-binary/diverse;  9 = Refused'

i20_iea_file[8 + nrow(df_i2_filtered)+1,] = c("","","","","","",'<list_end>')
# +2 the additional information
i20_iea_file[8 + nrow(df_i2_filtered)+2,] = c(text_end_i02,"","","","","","")
}



# file_a, file_b and iea_file
# file a will be made in the first and can be created from an ISCED level 02 or 1.
# I02 = 0 else I1 = 0 else empty file.
# I1 = 0 else I2 = 0 else empty file. 

files = list(is02_file,i20_file,i20_iea_file)

}