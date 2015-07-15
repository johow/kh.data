# kh.data
Examples from the Krummhörn family reconstitution study (see [Voland, 2000](http://www.researchgate.net/profile/Eckart_Voland/publication/255608852_Contributions_of_family_reconstitution_studies_to_evolutionary_reproductive_ecology/links/543283bf0cf22395f29c18de.pdf)) to use with the 'kinlab' package for R. Try this:

`library(devtools)`  
`install_github("kh.data", "johow")`  
`install_github("kinlab", "johow")`  
`library(kinlab)`  
`plot_pedigree(x=1570, evdat="1800-01-01",  df_ind=kh.data::kh_ind, ped=kh.data::kh_ped[["1570"]], evmat=kh.data::kh_mat)`  

 
