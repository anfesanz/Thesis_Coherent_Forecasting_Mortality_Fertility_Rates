popedu_rec <- popedu %>%
  select(-starts_with("edu")) %>%
  left_join(rec) %>%
  mutate(edu1=as.integer(edu1*pop), edu2=as.integer(edu2*pop), edu3=as.integer(edu3*pop), edu4=as.integer(edu4*pop)) 

popedu_rec_hf <- popedu %>%
  select(-starts_with("edu")) %>%
  left_join(rec_hf) %>%
  mutate(edu1=as.integer(edu1*pop), edu2=as.integer(edu2*pop), edu3=as.integer(edu3*pop), edu4=as.integer(edu4*pop)) 

#Test percentages
#%>% mutate(pop1=edu1+edu2+edu3+edu4, ratio=pop/pop1)

