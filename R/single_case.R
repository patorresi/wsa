#' Single case
#'
#' This function retrieves the allocation and set the samples.
#' @param x The data frame gathered will utilized the uploaded data frame.
#' @keywords allocation
#' @export
#' @examples
#' single_case()


single_case = function(x,y){
# This version of the function allocation_weights handles the case where listing 
# forms contain teachers from only one ISCED level. 
# First, I evaluate which ISCED level the listing form possesses.
doc_values = apply(x[,c(6,7,8)],2,FUN=function(x){as.numeric(sum(x) == 0)})

################################################################################
# Create output file for first file (ISCED 02 or ISCED 1)
################################################################################
# After the algorithm has completed its work, the program will create three new files. 
# Two of these are listing forms that the country will use with WinW3S,
# while the remaining one the country will uploaded in SharePoint.

# In this particular case, no allocation was required, 
# as all the teachers/staff come from the same ISCED level. 
# Therefore, we will keep all the teachers/staff and transfer 
# the information to a new format that WinW3S can read.

# First case ISCED 02

if(all(doc_values==c(1,0,0))){
  # (1) We gather all the cases listed in the adapted listing form. 
  df_a = x
  # (2) create an empty matrix with 8 rows for each one of the things will add them 
  # to the listing form. 
  # The listing form from ISCED 02 and ISCED 1&2 Have the same number of columns
  # but different number of rows (School coordinator require another row in the 
  # info section)
  i_a_header = as.data.frame(matrix(rep("",64), nrow = 8, ncol = 8))
  # (3) I add the information on the top of the listing form. 
  # ISCED 02 does not have the name of the school coordinator.
  text_title_i02 = 'TALIS Starting Strong 2024 MS  - [ISCED Level 02] Listing Form'
  i_a_header[1,1] =  text_title_i02
  i_a_header[3:5,1] = c("Country/Region",'ECEC Setting Name','ECEC Setting ID')
  i_a_header[3:5,3] = y[1:3,]
  # Columns names in the listing form.
  i_a_header[8,] = c('Name',
                    'Sequence Number',
                    'Sequence Number',
                    'Exemption',                  
                    'Leader Role',
                    'Staff Role',
                    'Year of Birth',
                    'Gender')
  # (4) Reorganize the order of the columns.
  df_a$Exemption = ""
  df_a_filtered = df_a[,c(1,2,3,13,9,10,4,5)]
  names(df_a_filtered) = names(i_a_header)[1:8]
  df_ia_file = rbind(i_a_header,df_a_filtered)
  # (5) The remaining point is to add the last row containing the code list_end.
  # I counted 8 rows (header values) + number of teacher/staff added to the file
  # (nrow()) and then we put the last row.
  # +1 blank space with the end line string
  df_ia_file[8 + nrow(df_a_filtered)+1,] = c("","","","","","",'<list_end>')

# Second case ISCED 1

}else if(all(doc_values==c(0,1,0)) | all(doc_values==c(0,0,1))){
  df_a = x
  # (6) Unlike the previous case, this file has 9 rows of information because 
  # ISCED 1 and 2 require the name of the school coordinator at the beginning.
  i_a_header = as.data.frame(matrix(rep("",63), nrow = 9, ncol = 7))
  text_title_i10 = 'TALIS 2024 - [ISCED Level 1&2] Teacher Listing Form'
  i_a_header[1,1] =  text_title_i10
  i_a_header[3:6,1] = c("TALIS Country/Region",'School Name','School ID','School Coordinator')
  i_a_header[3:6,3] = y[1:4,]
  i_a_header[9,] = c('Teacher Name',
                  'Sequence Number',
                  'Sequence Number',
                  'Exemption',
                  'Year of Birth',
                  'Gender',
                  'Main Subject Domain at [ISCED Level 1]')
  # (7) I create the column Exemption. This variable was used in the previous
  # TALIS cycle and was not deleted from the listing form template.
  df_a$Exemption = ""
  # (8) Add the filtered teachers
  df_a_filtered = df_a[,c(1,2,3,13,4,5,11)]
  names(df_a_filtered) = names(i_a_header)
  df_ia_file = rbind(i_a_header,df_a_filtered)
  df_ia_file[9 + nrow(df_a_filtered)+1,] = c("","","","","","","",'<list_end>')

# Third case ISCED 2

}else{
  # (9) Blank case. This case should not be required.
  # I leave just in case there was an exceptional case
  # Either way this file should be replaced with the
  # if function.
  df_ia_file = as.data.frame(matrix(rep("",56), nrow = 8, ncol = 7))
}

################################################################################
# Create output file for ISCED level 1 or ISCED level 2
################################################################################
# As with the previous "if" function, we create a file if the adapted 
# listing form includes teachers from ISCED level 1 or 2.

if(doc_values[3] == 1){
  df_b = x
  # Unlike the previous case, this file has 9 rows of information because 
  # ISCED 1 and 2 require the name of the school coordinator at the beginning.
  i_a_header = as.data.frame(matrix(rep("",63), nrow = 9, ncol = 7))
  text_title_i20  = 'TALIS 2024 MS - [ISCED Level 1&2] Teacher Listing Form'
  i_a_header[1,1] =  text_title_ib
  i_a_header[3:6,1] = c("TALIS Country/Region",'School Name','School ID','School Coordinator')
  i_a_header[3:6,3] = y[1:4,]
  i_a_header[9,] = c('Teacher Name',
                    'Sequence Number',
                    'Sequence Number',
                    'Exemption',
                    'Year of Birth',
                    'Gender',
                    'Main Subject Domain at [ISCED Level 2]')
  # Add the filtered teachers
  # First, we need to create a empty row with the hide column
  df_b$Exemption = ""
  df_b_filtered = df_b[,c(1,2,3,13,4,5,12)]
  names(df_b_filtered) = names(i_a_header)
  df_ib_file = rbind(i_a_header,df_b_filtered)
  # add the ending rows
  # 8 rows with the header values + number of teacher/staff added to the file
  # +1 blank space with the end line string
  # text_end_i02 = '??? Leader Role: 1 = Leader of this ECEC setting
  # ??? Staff Role: 1 = <Only leader (no pedagogical work)>; 2 = <Teacher>; 3 = <Assistant>; 4 = <Staff for individual children>; 
  # 5 = <Staff for special tasks>; 6 = <Intern>;  7 = <country-specific>; 8 = <country-specific>; 9 = <country-specific>; 10 = <country-specific>;   
  # 11 = <country-specific>; 12 = <country-specific>
  # ??? Year of Birth: YYYY;  9999 = Not specified
  # ??? Gender: 1 = Female;  2 = Male; 3 = Non-binary/diverse;  9 = Refused'
  df_ib_file[9 + nrow(df_b_filtered)+1,] = c("","","","","","",'<list_end>')
  # +2 the additional information
  # df_ib_file[8 + nrow(df_i2_filtered)+2,] = c(text_end_i02,"","","","","","")
}else if(doc_values[3] == 0 & doc_values[2] == 1){
  # ISCED 2
  df_b = x
  isb_header = as.data.frame(matrix(rep("",63), nrow = 9, ncol = 7))
  text_title_ib  = 'TALIS 2024 MS - [ISCED Level 1&2] Teacher Listing Form'
  isb_header[1,1] =  text_title_ib
  isb_header[3:6,1] = c("TALIS Country/Region",'School Name','School ID','School Coordinator')
  isb_header[3:6,3] = y[1:4,]
  isb_header[9,] = c('Teacher Name',
                    'Sequence Number',
                    'Sequence Number',
                    'Exemption',
                    'Year of Birth',
                    'Gender',
                    'Main Subject Domain at [ISCED Level 1]')
  df_b$Exemption = ""
  df_b_filtered = df_b[,c(1,2,3,13,4,5,11)]
  names(df_b_filtered) = names(isb_header)
  df_ib_file = rbind(isb_header,df_b_filtered)
  df_ib_file[9 + nrow(df_b_filtered)+1,] = c("","","","","","",'<list_end>')
}else{
  df_ib_file = as.data.frame(matrix(rep("",63), nrow = 9, ncol = 7))
}

################################################################################
# Create output file for IEA
################################################################################

{# iea file
# After the algorithm work. we create three files. Two will be used by WinW3S
# and the remainning one will be deliver to the IEA.
# After the draft is ready, now the ouput will be assemble. 

iea_file = x
iea_file[,1] = "ID"
colnames(iea_file) = c("Teacher Name",
"Sequence Number",
"Sequence Number",
"Year of Birth",
"Gender",
"[ISCED level 02]",
"[ISCED level 1]",
"[ISCED level 2]",
"Leader Role [ISCED level 02]",
"Staff Role [ISCED level 02]",
"Main Subject Domain at [ISCED Level 1]",
"Main Subject Domain at [ISCED Level 2]")
iea_file$ dist = 1
}


################################################################################
# Names for the files
################################################################################
ia_file_name = if(doc_values[1] == 1){
  "ISCED02"}else{"ISCED1"}
ib_file_name = if(doc_values[3] == 1){
  "ISCED2"}else{"ISCED1"}
# file_a, file_b and iea_file
# file a will be made in the first and can be created from an ISCED level 02 or 1.
# I02 = 0 else I1 = 0 else empty file.
# I1 = 0 else I2 = 0 else empty file. 
files = list(df_ia_file,df_ib_file,iea_file,ia_file_name,ib_file_name)
}
