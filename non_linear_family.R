

# Function to generate NL1 family of functions (non linearity in trignometric terms)

nl1<- function(dim,split,imp,resptype){


if (dim==12 & split==1){
  ncont<-3
  
  
} else if (dim==12 & split==2){
  
  ncont<-6
  
} else if (dim==12 & split==3){
  
  ncont<-9
  
} else if (dim==36 & split==1){
  
  ncont<-9
  
} else if (dim==36 & split ==2){
  
  ncont<-18
  
} else if (dim==36 & split==3){
  
  ncont<-27
  
} else if (dim==60 & split ==1){
  
  ncont<-15
  
} else if (dim==60 & split==2){
  
  ncont<-30
  
} else if (dim==60 & split==3){
  
  ncont<-45
  
} else (print("Wrong input"))


ncat<-dim-ncont

spl<-dim*imp


#impvar<-sample(1:dim,spl)
if (split==3 & imp==0.75){
  impvar_cont<-sample(1:ncont,round(0.75*spl))
  impvar_categ<-sample((ncont+1):dim,spl-round(0.75*spl))
  
  
} else if (split==1 & imp==0.75) {impvar_cont<-sample(1:ncont,spl-round(0.75*spl))
impvar_categ<-sample((ncont+1):dim,round(0.75*spl))



}else {impvar_cont<-sample(1:ncont,round(spl/2))
impvar_categ<-sample((ncont+1):dim,spl-round(spl/2))
}

###### Continuous terms
terms_cont<-c()
for (i in 1:length(impvar_cont)){
  terms_cont[i]<-paste0('x',impvar_cont[i])
  
}


#Generating polynomial terms
poly_terms_cont2<-c()
for(i in 1:length(terms_cont)){
  poly_terms_cont2[i]<-paste0(terms_cont[i],'^2')
  
}

poly_terms_cont3<-c()
for(i in 1:length(terms_cont)){
  poly_terms_cont3[i]<-paste0(terms_cont[i],'^3')
  
}

poly_terms_cont4<-c()
for(i in 1:length(terms_cont)){
  poly_terms_cont4[i]<-paste0(terms_cont[i],'^4')
  
}


# #Generating sin terms
# sin_terms<-c()
# for(i in 1:length(terms_cont)){
#   sin_terms[i]<-paste0('sin(',terms_cont[i])
#   
# }

#Closing paranthesis
# for (i in 1:length(sin_terms)){
#   sin_terms[i]<- paste0(sin_terms[i],')')
# }


#Generating interaction terms

int_terms_cont<-unlist(lapply(1, function(i) combn(terms_cont, 2, paste, collapse = "*")))


#All additive cont terms
cont_add<-list(terms_cont,poly_terms_cont2,poly_terms_cont3,poly_terms_cont4)
cont_add_df<-as.data.frame(do.call(rbind,cont_add))

#All terms cont (as chr vector)
#cont_all<-c(terms_cont,poly_terms_cont2,poly_terms_cont3)


######  Categorical terms
# terms_categ<-c()
# for (i in 1:length(impvar_categ)){
#   terms_categ[i]<-paste0('x',impvar_categ[i])
#   
# }

fourlevel<- impvar_categ > (dim-3)
twolevel<- impvar_categ < (dim-2)

fourlevel_cat<- impvar_categ[fourlevel]
twolevel_cat<- impvar_categ[twolevel]


# Two-level categorical variables
if (length(twolevel_cat)==0){
  terms_categ_2l<-NULL
  
} else {
terms_categ_2l<-c()
for (i in 1:length(twolevel_cat)){
  terms_categ_2l[i]<-paste0('x',twolevel_cat[i])
  
 }
}
# Four-level categorical variables

if (length(fourlevel_cat)==0){
  
  terms_categ_4l<-NULL
  
} else{

terms_categ_4l<-c()
for (i in 1:length(fourlevel_cat)){
  terms_categ_4l[i]<-paste0('x',fourlevel_cat[i])
  
  }
}

if(length(terms_categ_2l)==0){
  
  ol1<-NULL
  ol2<-NULL
  
  
} else { ol1<-c()


for( i in 1:length(terms_categ_2l)){
  
  
  ol1[i]<-paste0(terms_categ_2l[i],'==',1)
  
}

ol2<-c()
for( i in 1:length(terms_categ_2l)){
  
  
  ol2[i]<-paste0(terms_categ_2l[i],'==',2)
  
}
}


if (length(terms_categ_4l)==0){
   ol41<-NULL
   ol42<-NULL
   ol43<-NULL
   ol44<-NULL
  
  
} else {

ol41<-c()


for( i in 1:length(terms_categ_4l)){
  
  
  ol41[i]<-paste0(terms_categ_4l[i],'==',1)
  
}

ol42<-c()
for( i in 1:length(terms_categ_4l)){
  
  
  ol42[i]<-paste0(terms_categ_4l[i],'==',2)
  
}

ol43<-c()


for( i in 1:length(terms_categ_4l)){
  
  
  ol43[i]<-paste0(terms_categ_4l[i],'==',3)
  
}

ol44<-c()
for( i in 1:length(terms_categ_4l)){
  
  
  ol44[i]<-paste0(terms_categ_4l[i],'==',4)
  
}
}



categ_add<- c(ol1,ol2,ol41,ol42,ol43,ol44)

#Adding paranthesis
for (i in 1:length(categ_add)){
  categ_add[i]<- paste0('(',categ_add[i],')')
}



#All additive categorical terms
#categ_add<-list(ol1,ol2,ol41,ol42,ol43,ol44)
#categ_add_df<-as.data.frame(do.call(rbind,categ_add))




categ_add_2l<-list(ol1,ol2)
categ_add_2l_df<-as.data.frame(do.call(rbind,categ_add_2l))

categ_add_4l<-list(ol41,ol42,ol43,ol44)
categ_add_4l_df<-as.data.frame(do.call(rbind,categ_add_4l))


#Generating interaction terms
# int_terms_categ<-unlist(lapply(1, function(i) combn(categ_add, 2, paste, collapse = "*")))

impvar_terms<-c(terms_cont,terms_categ_2l,terms_categ_4l)

###### Generating equations
if (resptype==1){    # Additive
  
  
   sterms<-c()
    
     for (i in 1:ncol(cont_add_df)){
       
       sterms[i]<-sample(cont_add_df[,i],1)
       
     }
  
   coef_cont<-round(runif(length(sterms),1,3),2)
   coef_cat<-round(runif(length(categ_add),1,3),2)
   
   for (i in 1:length(sterms)){
         sterms[i]<- paste0('(',sterms[i],')')
      }
   
   t_cont<-paste(coef_cont,sterms,sep='*')
   t_categ<-paste(coef_cat,categ_add,sep='*')
   
   resp1<- c(t_cont,t_categ)
   
   func<- paste(resp1,collapse = '+')

  }  else if (resptype==2){        # Interactions (cont-cont)
  
    sterms<-c()
    
    for (i in 1:ncol(cont_add_df)){
      
      sterms[i]<-sample(cont_add_df[,i],1)
      
    }
  
    for (i in 1:length(sterms)){
      sterms[i]<- paste0('(',sterms[i],')')
    }  
    
    
  int_terms_cont<-unlist(lapply(1, function(i) combn(sterms, 2, paste, collapse = "*")))
  
  
  if (length(int_terms_cont)< 3){
  
    int_terms_resp2<- sample(int_terms_cont)
    
  } else {
  
  int_terms_resp2<- sample(int_terms_cont,3)                                              #Generating interaction terms
  }
  coef_int<- round(runif(length(int_terms_resp2),10,15),2)                                   # Co-ef for interaction terms
  coef_cat<-round(runif(length(categ_add),10,15),2)                                        # Co-ef for categorical terms
  coef_cont<- round(runif(length(sterms),10,15),2) 
  
  t_categ<-paste(coef_cat,categ_add,sep='*')
  t_int<- paste(coef_int,int_terms_resp2,sep='*')
  t_cont<- paste(coef_cont,sterms,sep='*')
  
  resp1<- c(t_int,t_categ,t_cont)
  
  func<- paste(resp1,collapse = '+')
  
  
  } else if (resptype==3){                                                                              # Interactions(cont-categ)
  
    
    sterms<-c()
    for (i in 1:ncol(cont_add_df)){
      
      sterms[i]<-sample(cont_add_df[,i],1) #sampling cont additive terms
      
    }
    
    if(length(categ_add_2l_df)==0){
      
      sterms2<-NULL
    } else {
    sterms2<-c()
    for (i in 1:ncol(categ_add_2l_df)){
      
      sterms2[i]<-sample(categ_add_2l_df[,i],1) #sampling 2 level categ terms
      
    }
    }
    
    if (length(categ_add_4l_df)==0){
      
      sterms3<-NULL
    } else {
    sterms3<-c()
    for (i in 1:ncol(categ_add_4l_df)){
      
      sterms3[i]<-sample(categ_add_4l_df[,i],1) #sampling 4 level categ terms
      
    }
    }
    
    
    if(length(sterms2)==0){        # Adding parenthesis 
      
      sterms2<- NULL
      
    } else {
      
      for (i in 1:length(sterms2)){
        sterms2[i]<- paste0('(',sterms2[i],')')
      }
    }  
    
    
    
    if(length(sterms3)==0){    # Adding paranthesis
      
      sterms3<- NULL
      
    } else {
    
    for (i in 1:length(sterms3)){
      sterms3[i]<- paste0('(',sterms3[i],')')
    }
    }  
    
    
    ct<-c(sterms2,sterms3)
    
  if (length(sterms)<length(ct)){
    
    ct2<- ct[1:length(sterms)]
    
    
  } else {
    
    ct2<- ct
    
  }
    
    
   tr<- c()    # Cont-categ interaction terms
   
   
   for (i in 1:length(sterms)){
     
     tr[i]<- paste(sterms[i],ct2[i],sep='*')
     
   } 
  
   
   
   ct3<- ct[-(1:length(sterms))]
   
   
   
   
   
   ca<-c()
   for (i in 1:ncol(cont_add_df)){
     
     ca[i]<-sample(cont_add_df[,i],1) #sampling cont additive terms
     
   }
    
   resp1<- c(ca,tr,ct3)
   coef<- round(runif(length(resp1),10,15),2)
   
   for (i in 1:length(resp1)){
     resp1[i]<- paste0('(',resp1[i],')')
   }
    
   fm<- paste(coef,resp1,sep='*')
   func<- paste(fm,collapse = '+')
   
  } else if (resptype==4){
  
    sterms<-c()
    for (i in 1:ncol(cont_add_df)){
      
      sterms[i]<-sample(cont_add_df[,i],1) #sampling cont additive terms
      
    }
    
    
    if(length(categ_add_2l_df)==0){
      
      sterms2<-NULL
    } else {
    
    
    sterms2<-c()
    for (i in 1:ncol(categ_add_2l_df)){
      
      sterms2[i]<-sample(categ_add_2l_df[,i],1) #sampling 2 level categ terms
      
      }
    }  
    
    if(length(categ_add_4l_df)==0){
      
      sterms3<-NULL
    } else {
    
    sterms3<-c()
    for (i in 1:ncol(categ_add_4l_df)){
      
      sterms3[i]<-sample(categ_add_4l_df[,i],1) #sampling 4 level categ terms
      
      }
    }  
    
   
    
    
     for (i in 1:length(sterms)){
      sterms[i]<- paste0('(',sterms[i],')')
    }
    
    
    if(length(sterms2)==0){        # Adding parenthesis 
      
      sterms2<- NULL
      
    } else {
      
      for (i in 1:length(sterms2)){
        sterms2[i]<- paste0('(',sterms2[i],')')
      }
    }  
    
    
    
    if(length(sterms3)==0){    # Adding paranthesis
      
      sterms3<- NULL
      
    } else {
      
      for (i in 1:length(sterms3)){
        sterms3[i]<- paste0('(',sterms3[i],')')
      }
    }  
    
    
    
    
    
    ct<- c(sterms2,sterms3)
    
    int_terms_categ<-unlist(lapply(1, function(i) combn(ct, 2, paste, collapse = "*")))   # Interaction terms
    
    
    if (length(int_terms_categ)< 3){
      
      int_terms_resp2<- sample(int_terms_categ)
      
    } else {
      
      int_terms_resp2<- sample(int_terms_categ,3)                                              #Generating interaction terms
    }
    
    
    

    
    
    
    
    at<- c(sterms,int_terms_resp2,categ_add)
    
    coef<-round(runif(length(at),10,15),2)
    
    fm<- paste(coef,at,sep='*')
    func<- paste(fm,collapse = '+')
    
} else (print("Wrong resp type"))

  op<- list(func,impvar_terms)

 return(op)

}





# Function to generate NL2 family of functions (non linearity in polynomial and exponential terms)

nl2<- function(dim,split,imp,resptype){
  
  
  if (dim==12 & split==1){
    ncont<-3
    
    
  } else if (dim==12 & split==2){
    
    ncont<-6
    
  } else if (dim==12 & split==3){
    
    ncont<-9
    
  } else if (dim==36 & split==1){
    
    ncont<-9
    
  } else if (dim==36 & split ==2){
    
    ncont<-18
    
  } else if (dim==36 & split==3){
    
    ncont<-27
    
  } else if (dim==60 & split ==1){
    
    ncont<-15
    
  } else if (dim==60 & split==2){
    
    ncont<-30
    
  } else if (dim==60 & split==3){
    
    ncont<-45
    
  } else (print("Wrong input"))
  
  
  ncat<-dim-ncont
  
  spl<-dim*imp
  
  
  #impvar<-sample(1:dim,spl)
  if (split==3 & imp==0.75){
    impvar_cont<-sample(1:ncont,round(0.75*spl))
    impvar_categ<-sample((ncont+1):dim,spl-round(0.75*spl))
    
    
  } else if (split==1 & imp==0.75) {impvar_cont<-sample(1:ncont,spl-round(0.75*spl))
  impvar_categ<-sample((ncont+1):dim,round(0.75*spl))
  
  } else if (dim==12 & imp==0.25 & resptype==3){
    
    impvar_cont<- sample(1:ncont,2)
    impvar_categ<- sample((ncont+1):dim,1)
    
  }  else if (dim==12 & imp==0.25 & resptype==4){
    
    impvar_cont<- sample(1:ncont,1)
    impvar_categ<- sample((ncont+1):dim,2)
    
  } else {impvar_cont<-sample(1:ncont,round(spl/2))
  impvar_categ<-sample((ncont+1):dim,spl-round(spl/2))
  }
  
  ###### Continuous terms
  terms_cont<-c()
  for (i in 1:length(impvar_cont)){
    terms_cont[i]<-paste0('x',impvar_cont[i])
    
  }
  
  
  #Generating polynomial terms
  poly_terms_cont2<-c()
  for(i in 1:length(terms_cont)){
    poly_terms_cont2[i]<-paste0(terms_cont[i],'^2')
    
  }
  
  poly_terms_cont3<-c()
  for(i in 1:length(terms_cont)){
    poly_terms_cont3[i]<-paste0(terms_cont[i],'^3')
    
  }
  
  poly_terms_cont4<-c()
  for(i in 1:length(terms_cont)){
    poly_terms_cont4[i]<-paste0(terms_cont[i],'^4')
    
  }
  
  
  
  
  # Generating sin terms
  sin_terms<-c()
  for(i in 1:length(terms_cont)){
    sin_terms[i]<-paste0('sin(',terms_cont[i])
    
  }
  
  # Closing paranthesis on exp terms
  for (i in 1:length(sin_terms)){
    sin_terms[i]<- paste0(sin_terms[i],')')
    
  }
  
  cos_terms<-c()
  for(i in 1:length(terms_cont)){
    cos_terms[i]<-paste0('cos(',terms_cont[i])
    
  }
  
  # Closing paranthesis on exp terms
  for (i in 1:length(cos_terms)){
    cos_terms[i]<- paste0(cos_terms[i],')')
    
  }
  
  
  #Generating interaction terms
  
  #int_terms_cont<-unlist(lapply(1, function(i) combn(terms_cont, 2, paste, collapse = "*")))
  
  
  #All additive cont terms
  cont_add<-list(terms_cont,poly_terms_cont2,poly_terms_cont3,sin_terms,cos_terms)
  cont_add_df<-as.data.frame(do.call(rbind,cont_add))
  
  #All terms cont (as chr vector)
  #cont_all<-c(terms_cont,poly_terms_cont2,poly_terms_cont3)
  
  
  ######  Categorical terms
  
  fourlevel<- impvar_categ > (dim-3)
  twolevel<- impvar_categ < (dim-2)
  
  fourlevel_cat<- impvar_categ[fourlevel]
  twolevel_cat<- impvar_categ[twolevel]
  
  
  # Two-level categorical variables
  if (length(twolevel_cat)==0){
    terms_categ_2l<-NULL
    
  } else {
    terms_categ_2l<-c()
    for (i in 1:length(twolevel_cat)){
      terms_categ_2l[i]<-paste0('x',twolevel_cat[i])
      
    }
  }
  # Four-level categorical variables
  
  if (length(fourlevel_cat)==0){
    
    terms_categ_4l<-NULL
    
  } else{
    
    terms_categ_4l<-c()
    for (i in 1:length(fourlevel_cat)){
      terms_categ_4l[i]<-paste0('x',fourlevel_cat[i])
      
    }
  }
  
  if(length(terms_categ_2l)==0){
    
    ol1<-NULL
    ol2<-NULL
    
    
  } else { ol1<-c()
  
  
  for( i in 1:length(terms_categ_2l)){
    
    
    ol1[i]<-paste0(terms_categ_2l[i],'==',1)
    
  }
  
  ol2<-c()
  for( i in 1:length(terms_categ_2l)){
    
    
    ol2[i]<-paste0(terms_categ_2l[i],'==',2)
    
  }
  }
  
  
  if (length(terms_categ_4l)==0){
    ol41<-NULL
    ol42<-NULL
    ol43<-NULL
    ol44<-NULL
    
    
  } else {
    
    ol41<-c()
    
    
    for( i in 1:length(terms_categ_4l)){
      
      
      ol41[i]<-paste0(terms_categ_4l[i],'==',1)
      
    }
    
    ol42<-c()
    for( i in 1:length(terms_categ_4l)){
      
      
      ol42[i]<-paste0(terms_categ_4l[i],'==',2)
      
    }
    
    ol43<-c()
    
    
    for( i in 1:length(terms_categ_4l)){
      
      
      ol43[i]<-paste0(terms_categ_4l[i],'==',3)
      
    }
    
    ol44<-c()
    for( i in 1:length(terms_categ_4l)){
      
      
      ol44[i]<-paste0(terms_categ_4l[i],'==',4)
      
    }
  }
  
  
  
  categ_add<- c(ol1,ol2,ol41,ol42,ol43,ol44)
  
  #Adding paranthesis
  for (i in 1:length(categ_add)){
    categ_add[i]<- paste0('(',categ_add[i],')')
  }
  
  
  
  #All additive categorical terms
  #categ_add<-list(ol1,ol2,ol41,ol42,ol43,ol44)
  #categ_add_df<-as.data.frame(do.call(rbind,categ_add))
  
  
  
  
  categ_add_2l<-list(ol1,ol2)
  categ_add_2l_df<-as.data.frame(do.call(rbind,categ_add_2l))
  
  categ_add_4l<-list(ol41,ol42,ol43,ol44)
  categ_add_4l_df<-as.data.frame(do.call(rbind,categ_add_4l))
  
  
  
  
  
  
  
  impvar_terms<-c(terms_cont,terms_categ_2l,terms_categ_4l)
  
  ################################ Generating equations
  if (resptype==1){                                                                         # Additive
    
    
    sterms<-c()
    
    for (i in 1:ncol(cont_add_df)){
      
      sterms[i]<-sample(cont_add_df[,i],1)
      
    }
    
    coef_cont<-round(runif(length(sterms),10,15),2)
    coef_cat<-round(runif(length(categ_add),10,15),2)
    
    for (i in 1:length(sterms)){
      sterms[i]<- paste0('(',sterms[i],')')
    }
    
    t_cont<-paste(coef_cont,sterms,sep='*')
    t_categ<-paste(coef_cat,categ_add,sep='*')
    
    resp1<- c(t_cont,t_categ)
    
    func<- paste(resp1,collapse = '+')
    
  } else if (resptype==2){        # Interactions (cont-cont)
    
    sterms<-c()
    
    for (i in 1:ncol(cont_add_df)){
      
      sterms[i]<-sample(cont_add_df[,i],1)
      
    }
    
    for (i in 1:length(sterms)){
      sterms[i]<- paste0('(',sterms[i],')')
    }  
    
    
    int_terms_cont<-unlist(lapply(1, function(i) combn(sterms, 2, paste, collapse = "*")))
    
    
    if (length(int_terms_cont)< 3){
      
      int_terms_resp2<- sample(int_terms_cont)
      
    } else {
      
      int_terms_resp2<- sample(int_terms_cont,3)                                              #Generating interaction terms
    }
    
    
    #Generating interaction terms
    coef_int<- round(runif(length(int_terms_resp2),10,15),2)                                   # Co-ef for interaction terms
    coef_cat<-round(runif(length(categ_add),10,15),2)                                        # Co-ef for categorical terms
    
    
    t_categ<-paste(coef_cat,categ_add,sep='*')
    t_int<- paste(coef_int,int_terms_resp2,sep='*')
    
    resp1<- c(t_int,t_categ)
    
    func<- paste(resp1,collapse = '+')
    
    
  } else if (resptype==3){                                                                              # Interactions(cont-categ)
    
    
    sterms<-c()
    for (i in 1:ncol(cont_add_df)){
      
      sterms[i]<-sample(cont_add_df[,i],1) #sampling cont additive terms
      
    }
    
    if(length(categ_add_2l_df)==0){
      
      sterms2<-NULL
    } else {
      sterms2<-c()
      for (i in 1:ncol(categ_add_2l_df)){
        
        sterms2[i]<-sample(categ_add_2l_df[,i],1) #sampling 2 level categ terms
        
      }
    }
    
    if (length(categ_add_4l_df)==0){
      
      sterms3<-NULL
    } else {
      sterms3<-c()
      for (i in 1:ncol(categ_add_4l_df)){
        
        sterms3[i]<-sample(categ_add_4l_df[,i],1) #sampling 4 level categ terms
        
      }
    }
    
    
    if(length(sterms2)==0){        # Adding parenthesis 
      
      sterms2<- NULL
      
    } else {
      
      for (i in 1:length(sterms2)){
        sterms2[i]<- paste0('(',sterms2[i],')')
      }
    }  
    
    
    
    if(length(sterms3)==0){    # Adding paranthesis
      
      sterms3<- NULL
      
    } else {
      
      for (i in 1:length(sterms3)){
        sterms3[i]<- paste0('(',sterms3[i],')')
      }
    }  
    
    
    ct<-c(sterms2,sterms3)
    
    if (length(sterms)<length(ct)){
      
      d<- length(sterms)
      ct2<- ct[1:length(sterms)]
      
      
    } else {
      
      d<- length(ct)
      ct2<- ct
      
    }
    
    
    tr<- c()    # Cont-categ interaction terms
    
    
    for (i in 1:d){
      
      tr[i]<- paste(sterms[i],ct2[i],sep='*')
      
    } 
    
    
    
    ct3<- ct[-(1:length(sterms))]
    
    
    
    
    
    ca<-c()
    for (i in 1:ncol(cont_add_df)){
      
      ca[i]<-sample(cont_add_df[,i],1) #sampling cont additive terms
      
    }
    
    resp1<- c(ca,tr,ct3)
    coef<- round(runif(length(resp1),10,15),2)
    
    for (i in 1:length(resp1)){
      resp1[i]<- paste0('(',resp1[i],')')
    }
    
    fm<- paste(coef,resp1,sep='*')
    func<- paste(fm,collapse = '+')
    
  } else if (resptype==4){                         # Categ-categ interaction
    
    sterms<-c()
    for (i in 1:ncol(cont_add_df)){
      
      sterms[i]<-sample(cont_add_df[,i],1) #sampling cont additive terms
      
    }
    
    
    if(length(categ_add_2l_df)==0){
      
      sterms2<-NULL
    } else {
      
      
      sterms2<-c()
      for (i in 1:ncol(categ_add_2l_df)){
        
        sterms2[i]<-sample(categ_add_2l_df[,i],1) #sampling 2 level categ terms
        
      }
    }  
    
    if(length(categ_add_4l_df)==0){
      
      sterms3<-NULL
    } else {
      
      sterms3<-c()
      for (i in 1:ncol(categ_add_4l_df)){
        
        sterms3[i]<-sample(categ_add_4l_df[,i],1) #sampling 4 level categ terms
        
      }
    }  
    
    
    
    
    for (i in 1:length(sterms)){
      sterms[i]<- paste0('(',sterms[i],')')
    }
    
    
    if(length(sterms2)==0){        # Adding parenthesis 
      
      sterms2<- NULL
      
    } else {
      
      for (i in 1:length(sterms2)){
        sterms2[i]<- paste0('(',sterms2[i],')')
      }
    }  
    
    
    
    if(length(sterms3)==0){    # Adding paranthesis
      
      sterms3<- NULL
      
    } else {
      
      for (i in 1:length(sterms3)){
        sterms3[i]<- paste0('(',sterms3[i],')')
      }
    }  
    
    
    
    
    
    ct<- c(sterms2,sterms3)
    
    int_terms_categ<-unlist(lapply(1, function(i) combn(ct, 2, paste, collapse = "*")))   # Interaction terms
    
    if (length(int_terms_categ)< 3){
      
      int_terms_resp2<- sample(int_terms_categ)
      
    } else {
      
      int_terms_resp2<- sample(int_terms_categ,3)                                              #Generating interaction terms
    }
    
    
    
    
    
    at<- c(sterms,int_terms_resp2,categ_add)
    
    coef<-round(runif(length(at),10,15),2)
    
    fm<- paste(coef,at,sep='*')
    func<- paste(fm,collapse = '+')
    
  } else (print("Wrong resp type"))
  
  op<- list(func, impvar_terms)
  
  return(op)
  
  
}









# Function to generate NL3 family of functions (non linearity combining NL1 and NL2)

nl3<- function(dim,split,imp,resptype){
  
  
  if (dim==12 & split==1){
    ncont<-3
    
    
  } else if (dim==12 & split==2){
    
    ncont<-6
    
  } else if (dim==12 & split==3){
    
    ncont<-9
    
  } else if (dim==36 & split==1){
    
    ncont<-9
    
  } else if (dim==36 & split ==2){
    
    ncont<-18
    
  } else if (dim==36 & split==3){
    
    ncont<-27
    
  } else if (dim==60 & split ==1){
    
    ncont<-15
    
  } else if (dim==60 & split==2){
    
    ncont<-30
    
  } else if (dim==60 & split==3){
    
    ncont<-45
    
  } else (print("Wrong input"))
  
  
  ncat<-dim-ncont
  
  spl<-dim*imp
  
  
  #impvar<-sample(1:dim,spl)
  if (split==3 & imp==0.75){
    impvar_cont<-sample(1:ncont,round(0.75*spl))
    impvar_categ<-sample((ncont+1):dim,spl-round(0.75*spl))
    
    
  } else if (split==1 & imp==0.75) {impvar_cont<-sample(1:ncont,spl-round(0.75*spl))
  impvar_categ<-sample((ncont+1):dim,round(0.75*spl))
  
  } else if (dim==12 & imp==0.25 & resptype==3){
    
    impvar_cont<- sample(1:ncont,2)
    impvar_categ<- sample((ncont+1):dim,1)
    
  }  else if (dim==12 & imp==0.25 & resptype==4){
    
    impvar_cont<- sample(1:ncont,1)
    impvar_categ<- sample((ncont+1):dim,2)
    
  } else {impvar_cont<-sample(1:ncont,round(spl/2))
  impvar_categ<-sample((ncont+1):dim,spl-round(spl/2))
  }
  
  
  ###### Continuous terms
  terms_cont<-c()
  for (i in 1:length(impvar_cont)){
    terms_cont[i]<-paste0('x',impvar_cont[i])
    
  }
  
  
  #Generating polynomial terms
  poly_terms_cont2<-c()
  for(i in 1:length(terms_cont)){
    poly_terms_cont2[i]<-paste0(terms_cont[i],'^2')
    
  }
  
  poly_terms_cont3<-c()
  for(i in 1:length(terms_cont)){
    poly_terms_cont3[i]<-paste0(terms_cont[i],'^3')
    
  }
  
  poly_terms_cont4<-c()
  for(i in 1:length(terms_cont)){
    poly_terms_cont4[i]<-paste0(terms_cont[i],'^4')
    
  }
  
  # Generating log terms
  logistic_terms<-c()
  for(i in 1:length(terms_cont)){
    logistic_terms[i]<-paste0('plogis(',terms_cont[i])
    
  }
  
  # Closing paranthesis on log terms
  for (i in 1:length(logistic_terms)){
    logistic_terms[i]<- paste0(logistic_terms[i],')')
    
  }
  
  
  # Generating log terms
  log_terms<-c()
  for(i in 1:length(terms_cont)){
    log_terms[i]<-paste0('log10(',terms_cont[i])
    
  }
  
  # Closing paranthesis on log terms
  for (i in 1:length(log_terms)){
    log_terms[i]<- paste0(log_terms[i],')')
    
  }
  
  exp_terms<-c()
  for(i in 1:length(terms_cont)){
    exp_terms[i]<-paste0('exp(',terms_cont[i])
    
  }
  
  # Closing paranthesis on exp terms
  for (i in 1:length(exp_terms)){
  exp_terms[i]<- paste0(exp_terms[i],')')
    
  }
  
  
  #Generating interaction terms
  
  #int_terms_cont<-unlist(lapply(1, function(i) combn(terms_cont, 2, paste, collapse = "*")))
  
  
  #All additive cont terms
  cont_add<-list(terms_cont,poly_terms_cont2,poly_terms_cont3,log_terms,exp_terms,logistic_terms)
  cont_add_df<-as.data.frame(do.call(rbind,cont_add))
  
  #All terms cont (as chr vector)
  #cont_all<-c(terms_cont,poly_terms_cont2,poly_terms_cont3)
  
  
  ######  Categorical terms
  
  fourlevel<- impvar_categ > (dim-3)
  twolevel<- impvar_categ < (dim-2)
  
  fourlevel_cat<- impvar_categ[fourlevel]
  twolevel_cat<- impvar_categ[twolevel]
  
  
  # Two-level categorical variables
  if (length(twolevel_cat)==0){
    terms_categ_2l<-NULL
    
  } else {
    terms_categ_2l<-c()
    for (i in 1:length(twolevel_cat)){
      terms_categ_2l[i]<-paste0('x',twolevel_cat[i])
      
    }
  }
  # Four-level categorical variables
  
  if (length(fourlevel_cat)==0){
    
    terms_categ_4l<-NULL
    
  } else{
    
    terms_categ_4l<-c()
    for (i in 1:length(fourlevel_cat)){
      terms_categ_4l[i]<-paste0('x',fourlevel_cat[i])
      
    }
  }
  
  if(length(terms_categ_2l)==0){
    
    ol1<-NULL
    ol2<-NULL
    
    
  } else { ol1<-c()
  
  
  for( i in 1:length(terms_categ_2l)){
    
    
    ol1[i]<-paste0(terms_categ_2l[i],'==',1)
    
  }
  
  ol2<-c()
  for( i in 1:length(terms_categ_2l)){
    
    
    ol2[i]<-paste0(terms_categ_2l[i],'==',2)
    
  }
  }
  
  
  if (length(terms_categ_4l)==0){
    ol41<-NULL
    ol42<-NULL
    ol43<-NULL
    ol44<-NULL
    
    
  } else {
    
    ol41<-c()
    
    
    for( i in 1:length(terms_categ_4l)){
      
      
      ol41[i]<-paste0(terms_categ_4l[i],'==',1)
      
    }
    
    ol42<-c()
    for( i in 1:length(terms_categ_4l)){
      
      
      ol42[i]<-paste0(terms_categ_4l[i],'==',2)
      
    }
    
    ol43<-c()
    
    
    for( i in 1:length(terms_categ_4l)){
      
      
      ol43[i]<-paste0(terms_categ_4l[i],'==',3)
      
    }
    
    ol44<-c()
    for( i in 1:length(terms_categ_4l)){
      
      
      ol44[i]<-paste0(terms_categ_4l[i],'==',4)
      
    }
  }
  
  
  
  categ_add<- c(ol1,ol2,ol41,ol42,ol43,ol44)
  
  #Adding paranthesis
  for (i in 1:length(categ_add)){
    categ_add[i]<- paste0('(',categ_add[i],')')
  }
  
  
  
  #All additive categorical terms
  #categ_add<-list(ol1,ol2,ol41,ol42,ol43,ol44)
  #categ_add_df<-as.data.frame(do.call(rbind,categ_add))
  
  
  
  
  categ_add_2l<-list(ol1,ol2)
  categ_add_2l_df<-as.data.frame(do.call(rbind,categ_add_2l))
  
  categ_add_4l<-list(ol41,ol42,ol43,ol44)
  categ_add_4l_df<-as.data.frame(do.call(rbind,categ_add_4l))
  
  impvar_terms<-c(terms_cont,terms_categ_2l,terms_categ_4l)
  
  ################################ Generating equations
  if (resptype==1){                                                                         # Additive
    
    
    sterms<-c()
    
    for (i in 1:ncol(cont_add_df)){
      
      sterms[i]<-sample(cont_add_df[,i],1)
      
    }
    
    coef_cont<-round(runif(length(sterms),10,15),2)
    coef_cat<-round(runif(length(categ_add),10,15),2)
    
    for (i in 1:length(sterms)){
      sterms[i]<- paste0('(',sterms[i],')')
    }
    
    t_cont<-paste(coef_cont,sterms,sep='*')
    t_categ<-paste(coef_cat,categ_add,sep='*')
    
    resp1<- c(t_cont,t_categ)
    
    func<- paste(resp1,collapse = '+')
    
  } else if (resptype==2){        # Interactions (cont-cont)
    
    sterms<-c()
    
    for (i in 1:ncol(cont_add_df)){
      
      sterms[i]<-sample(cont_add_df[,i],1)
      
    }
    
    for (i in 1:length(sterms)){
      sterms[i]<- paste0('(',sterms[i],')')
    }  
    
    
    int_terms_cont<-unlist(lapply(1, function(i) combn(sterms, 2, paste, collapse = "*")))
    
    
    if (length(int_terms_cont)< 3){
      
      int_terms_resp2<- sample(int_terms_cont)
      
    } else {
      
      int_terms_resp2<- sample(int_terms_cont,3)                                              #Generating interaction terms
    }
    
    
    #Generating interaction terms
    coef_int<- round(runif(length(int_terms_resp2),10,15),2)                                   # Co-ef for interaction terms
    coef_cat<-round(runif(length(categ_add),10,15),2)                                        # Co-ef for categorical terms
    
    
    t_categ<-paste(coef_cat,categ_add,sep='*')
    t_int<- paste(coef_int,int_terms_resp2,sep='*')
    
    resp1<- c(t_int,t_categ)
    
    func<- paste(resp1,collapse = '+')
    
    
  } else if (resptype==3){                                                                              # Interactions(cont-categ)
    
    
    sterms<-c()
    for (i in 1:ncol(cont_add_df)){
      
      sterms[i]<-sample(cont_add_df[,i],1) #sampling cont additive terms
      
    }
    
    if(length(categ_add_2l_df)==0){
      
      sterms2<-NULL
    } else {
      sterms2<-c()
      for (i in 1:ncol(categ_add_2l_df)){
        
        sterms2[i]<-sample(categ_add_2l_df[,i],1) #sampling 2 level categ terms
        
      }
    }
    
    if (length(categ_add_4l_df)==0){
      
      sterms3<-NULL
    } else {
      sterms3<-c()
      for (i in 1:ncol(categ_add_4l_df)){
        
        sterms3[i]<-sample(categ_add_4l_df[,i],1) #sampling 4 level categ terms
        
      }
    }
    
    
    if(length(sterms2)==0){        # Adding parenthesis 
      
      sterms2<- NULL
      
    } else {
      
      for (i in 1:length(sterms2)){
        sterms2[i]<- paste0('(',sterms2[i],')')
      }
    }  
    
    
    
    if(length(sterms3)==0){    # Adding paranthesis
      
      sterms3<- NULL
      
    } else {
      
      for (i in 1:length(sterms3)){
        sterms3[i]<- paste0('(',sterms3[i],')')
      }
    }  
    
    
    ct<-c(sterms2,sterms3)
    
    if (length(sterms)<length(ct)){
      
      d<- length(sterms)
      ct2<- ct[1:length(sterms)]
      
      
    } else {
      
      d<- length(ct)
      ct2<- ct
      
    }
    
    
    tr<- c()    # Cont-categ interaction terms
    
    
    for (i in 1:d){
      
      tr[i]<- paste(sterms[i],ct2[i],sep='*')
      
    } 
    
    
    
    ct3<- ct[-(1:length(sterms))]
    
    
    
    
    
    ca<-c()
    for (i in 1:ncol(cont_add_df)){
      
      ca[i]<-sample(cont_add_df[,i],1) #sampling cont additive terms
      
    }
    
    resp1<- c(ca,tr,ct3)
    coef<- round(runif(length(resp1),10,15),2)
    
    for (i in 1:length(resp1)){
      resp1[i]<- paste0('(',resp1[i],')')
    }
    
    fm<- paste(coef,resp1,sep='*')
    func<- paste(fm,collapse = '+')
    
  } else if (resptype==4){                         # Categ-categ interaction
    
    sterms<-c()
    for (i in 1:ncol(cont_add_df)){
      
      sterms[i]<-sample(cont_add_df[,i],1) #sampling cont additive terms
      
    }
    
    
    if(length(categ_add_2l_df)==0){
      
      sterms2<-NULL
    } else {
      
      
      sterms2<-c()
      for (i in 1:ncol(categ_add_2l_df)){
        
        sterms2[i]<-sample(categ_add_2l_df[,i],1) #sampling 2 level categ terms
        
      }
    }  
    
    if(length(categ_add_4l_df)==0){
      
      sterms3<-NULL
    } else {
      
      sterms3<-c()
      for (i in 1:ncol(categ_add_4l_df)){
        
        sterms3[i]<-sample(categ_add_4l_df[,i],1) #sampling 4 level categ terms
        
      }
    }  
    
    
    
    
    for (i in 1:length(sterms)){
      sterms[i]<- paste0('(',sterms[i],')')
    }
    
    
    if(length(sterms2)==0){        # Adding parenthesis 
      
      sterms2<- NULL
      
    } else {
      
      for (i in 1:length(sterms2)){
        sterms2[i]<- paste0('(',sterms2[i],')')
      }
    }  
    
    
    
    if(length(sterms3)==0){    # Adding paranthesis
      
      sterms3<- NULL
      
    } else {
      
      for (i in 1:length(sterms3)){
        sterms3[i]<- paste0('(',sterms3[i],')')
      }
    }  
    
    
    
    
    
    ct<- c(sterms2,sterms3)
    
    int_terms_categ<-unlist(lapply(1, function(i) combn(ct, 2, paste, collapse = "*")))   # Interaction terms
    
    if (length(int_terms_categ)< 3){
      
      int_terms_resp2<- sample(int_terms_categ)
      
    } else {
      
      int_terms_resp2<- sample(int_terms_categ,3)                                              #Generating interaction terms
    }
    
    
    
    
    
    at<- c(sterms,int_terms_resp2,categ_add)
    
    coef<-round(runif(length(at),10,15),2)
    
    fm<- paste(coef,at,sep='*')
    func<- paste(fm,collapse = '+')
    
  } else (print("Wrong resp type"))
  
  op<- list(func, impvar_terms)
  
  return(op)
  
  
}



  
  
 

