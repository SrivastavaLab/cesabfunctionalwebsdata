
######## Script to insert the new traits modalities created by Paula M. de Omena ##############
######## Our goal here is to insert new trait modalities that can be related to PREDATION DEFENSE
######## Feel free to improove the script, and disagree with the code I designated for the traits (values 0, 1, 2, 3)


#### Paula`s version of the script ready! #####

#### We still need to include the trait modalities: Body form and cohort production interval

######### Original Trait Matrix ###########




##### NEW TRAIT MODALITY --> MD = Morfological Defense

##### Within morphological defense (MD) we have:

### MD1 (none)

### MD2 (elongate tubercle) (0 = no tubercle, 1 = few tubercle, 2 = intermediate amount of tubercle, 3 = a lot of tubercle)

### MD3 (hairs) (0 = no hairs, 1 = few hairs, 2 = intermediate amount of hairs, 3 = dense hairs)

### MD4 (sclerotized spines) (0 = no spines, 1 = few spines, 2 = intermediate amount of spines, 3 = many spines)

### MD5 (dorsal plates) (0 = no plates, 1 = few plates (Example: Phylloicus), 2 = intermediate amount plates (half of the body), 3 = plates along all the segments (Example: Psychodidae))

### MD6 (sclerotized exoskeleton) (0, 1, 2 and  3)

### MD7 (shell) (0 or 3)

### MD8 (case/tube) (0 or 3)


add_MD_trait <- function(.traits_all_renamed){
  #### Here I create the new trait columns filled by "NA"
  trait.1 <- .traits_all_renamed %>%
    select(species_id, bwg_name, domain:subspecies) %>%
    cbind(data_frame(MD1 = NA_real_,
                     MD2 = NA_real_,
                     MD3 = NA_real_,
                     MD4 = NA_real_,
                     MD5 = NA_real_,
                     MD6 = NA_real_,
                     MD7 = NA_real_,
                     MD8 = NA_real_))

  trait.2 <- trait.1


  ##########################################################################

  ##### subclass Acari #####

  ########################################################################
  ### MD1 (none): (0)

  trait.2$MD1[trait.2$subclass=="Acari"]= 0

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$subclass=="Acari"]=0

  ### MD3 (hair) (1)
  trait.2$MD3[trait.2$subclass=="Acari"]= 1

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$subclass=="Acari"]=0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$subclass=="Acari"]=0

  ### MD6 (sclerotized exoskeleton) (3)
  trait.2$MD6[trait.2$subclass=="Acari"]=3

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$subclass=="Acari"]=0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$subclass=="Acari"]=0



  ##########################################################################

  ##### aff Drosophilidae #####

  ###########################################################################

  ### MD1 (none): (3)
  trait.2$MD1[trait.2$family=="aff. Drosophilidae"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="aff. Drosophilidae"]=0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="aff. Drosophilidae"]=0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="aff. Drosophilidae"]=0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="aff. Drosophilidae"]=0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="aff. Drosophilidae"]=0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$family=="aff. Drosophilidae"]=0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="aff. Drosophilidae"]=0


  ##########################################################################

  ##### Anisopodidae ######

  ###########################################################################

  ### MD1 (none):(3)
  trait.2$MD1[trait.2$family=="Anisopodidae"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Anisopodidae"]=0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Anisopodidae"]=0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Anisopodidae"]=0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Anisopodidae"]=0

  ### MD6 (sclerotized exoskeleton)(0)
  trait.2$MD6[trait.2$family=="Anisopodidae"]=0

  ### MD7 (Shell)(0)
  trait.2$MD7[trait.2$family=="Anisopodidae"]=0

  ### MD8 (case/tube)(0)
  trait.2$MD8[trait.2$family=="Anisopodidae"]=0

  ##########################################################################

  ##### Aulacigastridae #####

  ###########################################################################

  ##### I found a description of one species from the genus Aulacigaster at: Papp, L. 2008. Description of the immature stages and the adult female of )
  ##### Aulacigaster africana, the first known for the Afrotripical Aulacigastridae (Diptera: Schizophora). African invertebrates, 49:227-232.


  ### MD1 (none): (3)
  trait.2$MD1[trait.2$family=="Aulacigastridae"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Aulacigastridae"]=0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Aulacigastridae"]=0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Aulacigastridae"]=0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Aulacigastridae"]=0

  ### MD6 (sclerotized exoskeleton)(0)
  trait.2$MD6[trait.2$family=="Aulacigastridae"]=0

  ### MD7 (Shell)(0)
  trait.2$MD7[trait.2$family=="Aulacigastridae"]=0

  ### MD8 (case/tube)(0)
  trait.2$MD8[trait.2$family=="Aulacigastridae"]=0

  ##########################################################################

  ##### Axymyiidae #####

  ###########################################################################

  #### I particularly do not know this family, I found information (picture) here http://www.diptera.info/forum/viewthread.php?thread_id=26719

  ### MD1 (none): (3)
  trait.2$MD1[trait.2$family=="Axymyiidae"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Axymyiidae"]=0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Axymyiidae"]=0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Axymyiidae"]=0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Axymyiidae"]=0

  ### MD6 (sclerotized exoskeleton)(0)
  trait.2$MD6[trait.2$family=="Axymyiidae"]=0

  ### MD7 (Shell)(0)
  trait.2$MD7[trait.2$family=="Axymyiidae"]=0

  ### MD8 (case/tube)(0)
  trait.2$MD8[trait.2$family=="Axymyiidae"]=0



  ##########################################################################

  ##### Canacidae #####


  ###########################################################################

  ### information from "Pinho L. C. Guia on-line de identifica??o de larvas de Insetos Aquaticos do Estado de Sao Paulo: Diptera"
  ### I did not find much information about larvae from this family, if some of you know more details regarding defense, please tell me.


  ### MD1 (none): (3)
  trait.2$MD1[trait.2$family=="Canacidae"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Canacidae"]=0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Canacidae"]=0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Canacidae"]=0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Canacidae"]= 0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="Canacidae"]=0

  ### MD7 (Shell) )(0)
  trait.2$MD7[trait.2$family=="Canacidae"]=0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="Canacidae"]=0



  ###########################################################################

  ####### Cecidomyiidae ########

  ###########################################################################

  ### MD1 (none): (3)
  trait.2$MD1[trait.2$family=="Cecidomyiidae"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Cecidomyiidae"]= 0

  ### MD3 (hair) 0)
  trait.2$MD3[trait.2$family=="Cecidomyiidae"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Cecidomyiidae"]= 0

  ### MD5 (dorsal plates) 0)
  trait.2$MD5[trait.2$family=="Cecidomyiidae"]= 0

  ### MD6 (sclerotized exoskeleton)(0)
  trait.2$MD6[trait.2$family=="Cecidomyiidae"]= 0

  ### MD7 (Shell)0)
  trait.2$MD7[trait.2$family=="Cecidomyiidae"]= 0

  ### MD8 (case/tube)
  trait.2$MD8[trait.2$family=="Cecidomyiidae"]= 0


  ###########################################################################

  ####### Ceratopogonidae ########

  ###########################################################################
  ### genus Bezzia, Culicoides, Sphaeromias, Stilobezzia, Atrichopogon, Forcipomyia


  #####
  ##### NA  ==>## First I inserted zero value for what we know that never occur in Ceratopogonidae.

  ### MD5, MD6, MD7 AND MD8 ARE EQUAL FOR ALL CERATOPOGONIDAE
  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Ceratopogonidae"]= 0

  ### MD6 (sclerotized exoskeleton)(0)
  trait.2$MD6[trait.2$family=="Ceratopogonidae"]= 0

  ### MD7 (Shell)(0)
  trait.2$MD7[trait.2$family=="Ceratopogonidae"]= 0

  ### MD8 (case/tube)(0)
  trait.2$MD8[trait.2$family=="Ceratopogonidae"]= 0


  #####
  ###### Bezzia
  #####

  ### MD1 (none): (3)
  trait.2$MD1[trait.2$genus =="Bezzia"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$genus=="Bezzia"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$genus=="Bezzia"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$genus=="Bezzia"]= 0


  #####
  #### Culicoides
  #####

  ### MD1 (none):  (3)
  trait.2$MD1[trait.2$genus =="Culicoides"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$genus=="Culicoides"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$genus=="Culicoides"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$genus=="Culicoides"]= 0


  #####
  ##### Sphaeromias (from: http://bugguide.net/node/view/617624/bgimage)
  #####

  ### MD1 (none): (3
  trait.2$MD1[trait.2$genus =="Sphaeromias"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$genus=="Sphaeromias"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$genus=="Sphaeromias"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$genus=="Sphaeromias"]= 0



  ######
  ###### Stilobezzia ==> I did not find much information on the morphology of Stilobezzia larvae; please let me know if there are some defense structure.

  ### MD1 (none): (3)
  trait.2$MD1[trait.2$genus =="Stilobezzia"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$genus=="Stilobezzia"]= 0

  ### MD3 (hair) 0)
  trait.2$MD3[trait.2$genus=="Stilobezzia"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$genus=="Stilobezzia"]= 0

  ##We have some NA for Ceratopogonidae Subfamily and Genus, and most of them are classified as prey.
  ##Thus, I just code them as members of Forcipomyiinae Subfamily (the Mode).
  ### MD1 (none): (0 )
  trait.2$MD1[trait.2$family=="Ceratopogonidae" & is.na(trait.2$subfamily)] = 0

  ### MD2 (elongated tubercle) (2)
  trait.2$MD2[trait.2$family=="Ceratopogonidae" & is.na(trait.2$subfamily)] = 2

  ### MD3 (hair) (2)
  trait.2$MD3[trait.2$family=="Ceratopogonidae" & is.na(trait.2$subfamily)] = 2

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Ceratopogonidae" & is.na(trait.2$subfamily)] = 0


  ####
  #### Subfamily Forcipomyiinae ==> Segments of the body with conspicuous dorsal tubercules and/or setae.
  ####

  ### MD1 (none): (0 )
  trait.2$MD1[trait.2$subfamily =="Forcipomyiinae"]= 0

  ### MD2 (elongated tubercle) (2)
  trait.2$MD2[trait.2$subfamily=="Forcipomyiinae"]= 2

  ### MD3 (hair) (2)
  trait.2$MD3[trait.2$subfamily=="Forcipomyiinae"]= 2

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$subfamily=="Forcipomyiinae"]= 0


  #####
  #####Atrichopogon
  #####

  ### MD1 (none): (0)
  trait.2$MD1[trait.2$genus =="Atrichopogon"]= 0

  ### MD2 (elongated tubercle) (3)
  trait.2$MD2[trait.2$genus=="Atrichopogon"]= 3

  ### MD3 (hair) (2)
  trait.2$MD3[trait.2$genus=="Atrichopogon"]= 2

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$genus=="Atrichopogon"]= 0


  #####
  #####Forcipomyia
  #####

  ### MD1 (none): (0)
  trait.2$MD1[trait.2$genus =="Forcipomyia"]= 0

  ### MD2 (elongated tubercle) (2)
  trait.2$MD2[trait.2$genus=="Forcipomyia"]= 2

  ### MD3 (hair) (3)
  trait.2$MD3[trait.2$genus=="Forcipomyia"]= 3

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$genus=="Forcipomyia"]= 0


  ####
  #### Subfamily Leptoconopinae ==> information from Tachet
  ####

  ### MD1 (none): (3)
  trait.2$MD1[trait.2$subfamily =="Leptoconopinae"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$subfamily=="Leptoconopinae"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$subfamily=="Leptoconopinae"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$subfamily=="Leptoconopinae"]= 0

  ####
  #### Subfamily Dasyheleniae ==> information from Tachet
  ####

  ### MD1 (none): (0)
  trait.2$MD1[trait.2$subfamily =="Dasyheleniae"]= 0

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$subfamily=="Dasyheleniae"]= 0

  ### MD3 (hair) (2)
  trait.2$MD3[trait.2$subfamily=="Dasyheleniae"]= 2

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$subfamily=="Dasyheleniae"]= 0

  ####
  #### Subfamily Ceratopogoninae ==> information from Tachet
  ####

  ### MD1 (none): (3)
  trait.2$MD1[trait.2$subfamily =="Ceratopogoninae"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$subfamily=="Ceratopogoninae"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$subfamily=="Ceratopogoninae"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$subfamily=="Ceratopogoninae"]= 0


  #########################################################################################

  ##### Family Chironomidae #####

  #########################################################################################

  ### genus: Chironomus, Harnischia, Paratanytarsus, Polypedilum, Stempellinella, Tanytarsus,
  ### Corynoneura, Eukiefferiella, Gravatamberus, Limnophyes, Metriocnemus, Phytotelmatocladius,
  ### Rheocricotopus, Smittia, Boreochlus, Larsia, Monopelopia and also NAs

  ### we have the Diptera.447 (black head chironomid) = it could be a sciaridae?

  ### MD1 (none):
  trait.2$MD1[trait.2$family=="Chironomidae"]=0 ### first I inserted 0 for all Chironomidae, then I substitute by
  ### the correct value for each subfamily/genus that are different from 0

  trait.2$MD1[trait.2$genus=="Stenochironomus"]=3
  trait.2$MD1[trait.2$genus=="Tanytarsus"]=0
  trait.2$MD1[trait.2$subfamily=="Tanypodinae"]=3

  trait.2$MD1[trait.2$bwg_name == "Diptera.447"]=3 ### Diptera.447 (black head chironomid); 3 for no defense.

  ### MD2 (Elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Chironomidae"]=0 ### first I inserted 0 for all Chironomidae, then I substitute by
  ### the correct value for the subfamily/genus that are different from 0

  trait.2$MD2[trait.2$genus=="Chironomus"]=1

  trait.2$MD2[trait.2$bwg_name == "Diptera.447"]=0 ### Diptera.447 (black head chironomid) (The mode)

  ### MD3 (hair): Most of the Chironomidae from bromeliads has no hair exceptions: Lymnophies that have vey few hairs
  ### If there are other Chironomids with hairs in the trait database, please tell me

  trait.2$MD3[trait.2$family=="Chironomidae"]=0
  trait.2$MD3[trait.2$genus=="Limnophyes"]=1


  trait.2$MD3[trait.2$bwg_name == "Diptera.447"]=0 ### Diptera.447 (black head chironomid); 0 is the mode

  ### MD4 (Sclaritized spines) (0)
  trait.2$MD4[trait.2$family=="Chironomidae"]=0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Chironomidae"]=0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="Chironomidae"]=0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$family=="Chironomidae"]=0

  ### MD8 (Case/Tube) (0,1,2,3)

  ### I used the values of the trait LO7 (tube builter - original trait matrix) and inserted here. Why I did that?
  ### I saw that for some the species of Chironomidae (that are: Diptera.484, Diptera.5, Diptera.55,Diptera.56
  ### Diptera.57,Diptera.59,Diptera.60,Diptera.61,Diptera.62,Diptera.63,Diptera.70,Diptera.71,Diptera.72
  ### Diptera.73,Diptera.74,Diptera.75,Diptera.76,Diptera.77) we do not have information regarding SUBFAMILY OR GENUS.
  ### However, in the original trait matrix, they are classified as tube builder with the value 3 (Fuzzy code).
  ### I believe that the number 3 were given for these species according to specialist knowledge of the local fauna. So,
  ### I decided to follow the original trait matrix to avoid inconsistencies.

  ### OK: Diane explained that she code the organisms with no information in a deeper taxonomic level according to the mode.


  trait.2$MD8[trait.2$family=="Chironomidae"]=3 ### first I inserted 3 for all Chironomidae, then I substitute by
  ### the correct value for each genus/subfamily
  trait.2$MD8[trait.2$subfamily=="Chironominae"]=3
  trait.2$MD8[trait.2$subfamily=="Orthocladiinae"]=1
  trait.2$MD8[trait.2$subfamily=="Podonominae"]=1
  trait.2$MD8[trait.2$subfamily=="Tanypodinae"]=0

  trait.2$MD8[trait.2$genus=="Chironomus"]=3
  trait.2$MD8[trait.2$genus=="Harnischia"]=3
  trait.2$MD8[trait.2$genus=="Paratanytarsus"]=3
  trait.2$MD8[trait.2$genus=="Polypedilum"]=3
  trait.2$MD8[trait.2$genus=="Stenochironomus"]=0
  trait.2$MD8[trait.2$genus=="Tanytarsus"]=3 #??


  trait.2$MD8[trait.2$bwg_name == "Diptera.447"]=0 ### I do not think that Diptera.447 (black head chironomid) should be a tube builter
  ## we should confirm it it is Sciaridae or chironomid.


  #########################################################################################

  ####### Family Culicidae ########

  #########################################################################################

  ###### Family - Culicidae; GENUS - Aedes, Anopheles, Culex, Haemagogus, Limatus, Orthopodomyia, Runchomyia, Toxorhynchites, Wyeomyia and we also have NA (for family and genus)
  ###### Informations of Culicidae from the "Neotropical Culicidae" book - by J. Lane; from Personal Observations (Cardoso/Picinguaba)


  ### Organisms from the family Culicidae only have hairs as defense modality

  ### MD1 (none): 0
  trait.2$MD1[trait.2$family=="Culicidae"]=0

  ### MD2 (Elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Culicidae"]=0

  ### MD4 (Sclaritized spines) (0)
  trait.2$MD4[trait.2$family=="Culicidae"]=0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Culicidae"]=0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="Culicidae"]=0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$family=="Culicidae"]=0

  ### MD8 (Case/Tube) (0)
  trait.2$MD8[trait.2$family=="Culicidae"]=0

  #### MD3 (Hairs)
  ### Aedes: more often intermediate amount of hairs (2)
  trait.2$MD3[trait.2$genus=="Aedes"]=2

  ### Anopheles: more often dense amount of hairs (3)
  trait.2$MD3[trait.2$genus=="Anopheles"]=3

  ### Anopheles: more often dense amount of hairs (3)
  trait.2$MD3[trait.2$subfamily=="Anophelinae"]=3

  ### Culex: more often have intermediate amount of hairs (2)
  trait.2$MD3[trait.2$genus=="Culex"]=2

  ### Haemagogus: more often intermediate amount of hairs (2)
  trait.2$MD3[trait.2$genus=="Haemagogus"]=2

  ### Limatus (we have just one species in the trait matrix): few hairs (1)
  trait.2$MD3[trait.2$genus=="Limatus"]=1

  ### Orthopodomyia: more often intermediate amount of hairs (2)
  trait.2$MD3[trait.2$genus=="Orthopodomyia"]=2

  ### Runchomyia: more often have few hairs (1)
  trait.2$MD3[trait.2$genus=="Runchomyia"]=1

  ### Wyeomyia: more often DENSE HAIRS (3)
  trait.2$MD3[trait.2$genus=="Wyeomyia"]=3

  ### Toxorhynchites: more often DENSE HAIRS (3)
  trait.2$MD3[trait.2$genus=="Toxorhynchites"]=3

  #### For the NA values I will work as Diane did, I am going to use the mode. Culex is the most common genus,
  #### thus I inteted the value 2 (= Culex - hair modality).
  ### Culicid = Genus = NA
  trait.2$MD3[trait.2$family=="Culicidae" & is.na(trait.2$genus)]=2

  ## Add a trait for a incompletely indentified genus
  trait.2$MD3[trait.2$genus=="Anopheles_or_Wyeomia_or_Culex"]=2


  ##########################################################################

  ##### Family Psychodidae  #####

  ###########################################################################

  ##### (I know that there is a great variation in this family, however, we do not have good identification
  #####  of the genus)


  ### MD1 (none): 0
  trait.2$MD1[trait.2$family=="Psychodidae"]=0

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Psychodidae"]=0

  ### MD3 (hair): For Telmatoscopus I think is intermediate amount of hairs (2)

  trait.2$MD3[trait.2$family=="Psychodidae"]=3
  trait.2$MD3[trait.2$genus=="Telmatoscopus"]=2

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Psychodidae"]=0

  ### MD5 (dorsal plates) (2) ==> "thoracic and abdominal segments with 2-3 secondary subdivisions, some or all carrying dorsal sclerotized plaques" (Key for diptera - L. C. Pinho)

  trait.2$MD5[trait.2$family=="Psychodidae"]=2

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="Psychodidae"]=0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$family=="Psychodidae"]=0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="Psychodidae"]=0


  ##########################################################################

  ##### Ord - Odonata #####

  ###########################################################################

  ### MD1 (none): 0
  trait.2$MD1[trait.2$ord=="Odonata"]= 0

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$ord=="Odonata"]=0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$ord=="Odonata"]=0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$ord=="Odonata"]=0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$ord=="Odonata"]= 0

  ### MD6 (sclerotized exoskeleton)(2)
  trait.2$MD6[trait.2$ord=="Odonata"]=2 ### I considered the value 2 because it is not as hard as the exoskeleton of and adult.

  ### MD7 (Shell)
  trait.2$MD7[trait.2$ord=="Odonata"]=0

  ### MD8 (case/tube)
  trait.2$MD8[trait.2$ord=="Odonata"]=0


  #########################################################################

  ###### Family Corethrellidae ######

  ##########################################################################


  ### MD1 (none): 0
  trait.2$MD1[trait.2$family=="Corethrellidae"]= 0

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Corethrellidae"]= 0

  ### MD3 (hair) (2)
  trait.2$MD3[trait.2$family=="Corethrellidae"]= 2

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Corethrellidae"]= 0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Corethrellidae"]= 0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="Corethrellidae"]= 0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$family=="Corethrellidae"]=0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="Corethrellidae"]=0



  #########################################################################

  ###### Family Dolichopodidae ######

  ##########################################################################


  ### MD1 (none): (3)
  trait.2$MD1[trait.2$family=="Dolichopodidae"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Dolichopodidae"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Dolichopodidae"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Dolichopodidae"]= 0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Dolichopodidae"]= 0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="Dolichopodidae"]= 0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$family=="Dolichopodidae"]=0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="Dolichopodidae"]=0


  ### two morphospecies with wrong spelling
  ### MD1 (none): (3)
  trait.2$MD1[trait.2$family=="Dolychopodidae"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Dolychopodidae"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Dolychopodidae"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Dolychopodidae"]= 0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Dolychopodidae"]= 0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="Dolychopodidae"]= 0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$family=="Dolychopodidae"]=0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="Dolychopodidae"]=0


  ##########################################################################

  ##### Family - Empididae #####

  ###########################################################################


  ### MD1 (none): (3)
  trait.2$MD1[trait.2$family=="Empididae"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Empididae"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Empididae"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Empididae"]= 0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Empididae"]= 0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="Empididae"]= 0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$family=="Empididae"]=0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="Empididae"]=0

  ###########################################################################

  ####### Coleoptera  ########

  ###########################################################################

  ### traits shared by all coleoptera
  ### MD1 (none): 0
  trait.2$MD1[trait.2$ord=="Coleoptera"]= 0

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$ord=="Coleoptera"]= 0  ### This may change for aquatic Coleoptera, but I
  ### inserted the correct value for each family/genus (code below).
  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$ord=="Coleoptera"]= 0 ### This may change for aquatic Coleoptera, but I
  ### inserted the correct value for each family/genus (code below).
  ### MD6 (sclerotized exoskeleton)(3)
  trait.2$MD6[trait.2$ord=="Coleoptera"]= 3 ### it will change for some aquatic forms

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$ord=="Coleoptera"]= 0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$ord=="Coleoptera"]= 0


  ########## Coleoptera - Terrestrial

  ###
  ###Genus Platycrepidius
  ###

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$genus=="Platycrepidius"]= 0

  ### MD4 (sclerotized spines) (1)
  trait.2$MD4[trait.2$genus=="Platycrepidius"]= 1


  ####### Carabidae ########

  ### MD3 (hair) (1)
  trait.2$MD3[trait.2$family=="Carabidae"]= 1

  ### MD4 (sclerotized spines) (1)
  trait.2$MD4[trait.2$family=="Carabidae"]= 1

  ##### coleoptera, Family = NA
  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$ord=="Coleoptera"& is.na(trait.2$family)]= 0

  ### MD4 (sclerotized spines) (1)
  trait.2$MD4[trait.2$ord=="Coleoptera" & is.na(trait.2$family)]= 1


  ######### Coleoptera - AQUATIC #####################

  #### Family Dytiscidae - here we have both, adult and larvae. However, most of them are adult.

  ### MD3 (hair) (1)
  trait.2$MD3[trait.2$family=="Dytiscidae"]= 1

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$ord=="Coleoptera"]= 0

  ### MD4 (sclerotized spines) (1)
  trait.2$MD4[trait.2$family=="Dytiscidae"]= 1

  ### MD5 (dorsal plates) (2)
  trait.2$MD5[trait.2$ord=="Dytiscidae"]= 2 ### larvae has dorsal plates, but they are very rare in bromeliads


  ### family Curculionidae
  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Curculionidae"]= 0

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Curculionidae"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Curculionidae"]= 0


  ### Elateridae, genus NA (aquatic) (larvae ?)

  ### MD3 (hair) (1)
  trait.2$MD3[trait.2$family=="Elateridae" & is.na(trait.2$genus)]= 1

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$ord=="Elateridae"& is.na(trait.2$genus)]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Elateridae"& is.na(trait.2$genus)]= 0

  ### MD5 (dorsal plates) (2)
  trait.2$MD5[trait.2$ord=="Elateridae"& is.na(trait.2$genus)]= 2

  ### MD6 (sclerotized exoskeleton)(2)
  trait.2$MD6[trait.2$ord=="Elateridae"& is.na(trait.2$genus)]= 2


  ### Elmidae (the aquatic are all larvae)

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Elmidae"]= 0

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$ord=="Elmidae"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Elmidae"]= 0

  ### MD5 (dorsal plates) (3)
  trait.2$MD5[trait.2$ord=="Elmidae"]= 3

  ### MD6 (sclerotized exoskeleton)(2)
  trait.2$MD6[trait.2$ord=="Elmidae"]= 2


  ### Hydrophilidae (we have both adult and larvae)

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Hydrophilidae"]= 0

  ### MD2 (elongated tubercle) (1)
  trait.2$MD2[trait.2$ord=="Hydrophilidae"]= 1   # some species of this family have elongated tubercles; I do not know if the bromeliad ones have

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Hydrophilidae"]= 0

  ### MD5 (dorsal plates) (1)
  trait.2$MD5[trait.2$ord=="Hydrophilidae"]= 1 ### (3 segments of larvae' body)

  ### MD6 (sclerotized exoskeleton)(3)
  trait.2$MD6[trait.2$ord=="Hydrophilidae"]= 3 ### (adults)


  ### Lampyridae (just larvae as aquatic form)

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Lampyridae"]= 0

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$ord=="Lampyridae"]= 0

  ### MD4 (sclerotized spines) (1)
  trait.2$MD4[trait.2$family=="Lampyridae"]= 1 # some species of this family have esclerotized spines; I do not know if the bromeliad ones have

  ### MD5 (dorsal plates) (3)
  trait.2$MD5[trait.2$ord=="Lampyridae"]= 3

  ### MD6 (sclerotized exoskeleton)(2)
  trait.2$MD6[trait.2$ord=="Lampyridae"]= 2


  ## Ptilodactylidae (just larvae as aquatic form)  Larval body is elongate, cylindrical and sclerotized

  ### MD3 (hair) (1)
  trait.2$MD3[trait.2$family=="Ptilodactylidae"]= 1

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$ord=="Ptilodactylidae"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Ptilodactylidae"]= 0

  ### MD5 (dorsal plates) (1)
  trait.2$MD5[trait.2$ord=="Ptilodactylidae"]= 1 # first segment only

  ### MD6 (sclerotized exoskeleton)(2)
  trait.2$MD6[trait.2$ord=="Ptilodactylidae"]= 2


  ## Scirtidae (I think thy are all - larvae)

  ### MD3 (hair) (1)
  trait.2$MD3[trait.2$family=="Scirtidae"]= 1

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$ord=="Scirtidae"]= 0

  ### MD4 (sclerotized spines) (1)
  trait.2$MD4[trait.2$family=="Scirtidae"]= 0

  ### MD5 (dorsal plates) (3)
  trait.2$MD5[trait.2$ord=="Scirtidae"]= 3

  ### MD6 (sclerotized exoskeleton)(1)
  trait.2$MD6[trait.2$ord=="Scirtidae"]= 1



  ###### Coleoptera aquatic - Family and Subfamily = NA - (the mode is scirtidae)

  ### MD3 (hair) (1)
  trait.2$MD3[trait.2$ord=="Coleoptera" & trait.2$realm =="aquatic" & is.na(trait.2$family)]= 1

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$ord=="Coleoptera" & trait.2$realm =="aquatic"& is.na(trait.2$family)]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$ord=="Coleoptera" & trait.2$realm =="aquatic"& is.na(trait.2$family)]= 0

  ### MD5 (dorsal plates) (3)
  trait.2$MD5[trait.2$ord=="Coleoptera" & trait.2$realm =="aquatic"& is.na(trait.2$family)]= 3

  ### MD6 (sclerotized exoskeleton)(1)
  trait.2$MD6[trait.2$ord=="Coleoptera" & trait.2$realm =="aquatic"& is.na(trait.2$family)]= 1



  ##########################################################################

  ##### subphylum - Crustacea #####

  ###########################################################################
  ### Families: Chydoridae, Daphnidae, Cyclopidae, Canthocamptidae, Candonidae, Cyprididae, Limnocytheridae

  ######
  ##### Class Ostracoda #####
  ######
  ### MD1 (none): (0)
  trait.2$MD1[trait.2$class=="Ostracoda"]= 0

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$class=="Ostracoda"]=0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$class=="Ostracoda"]=0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$class=="Ostracoda"]=0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$class=="Ostracoda"]= 0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$class=="Ostracoda"]=0

  ### MD7 (Shell) (3)
  trait.2$MD7[trait.2$class=="Ostracoda"]=3 ###Ostracoda is the only group that will have a shell, and this
  ###group did not have a shell. The "shell" is a projection of their exoskeleton.
  ###So, instead of shell, Gustavo think that we should probably put 3 for sclerotized
  ###exoskeleton and remove this trait.

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$class=="Ostracoda"]=0


  ######
  ##### subclass Copepoda #####
  #####

  ### MD1 (none): 0
  trait.2$MD1[trait.2$subclass=="Copepoda"]= 0

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$subclass=="Copepoda"]= 0

  ### MD3 (hair) (1)
  trait.2$MD3[trait.2$subclass=="Copepoda"]= 1

  ### MD4 (sclerotized spines) (1)
  trait.2$MD4[trait.2$subclass=="Copepoda"]= 1

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$subclass=="Copepoda"]= 0

  ### MD6 (sclerotized exoskeleton) (1)
  trait.2$MD6[trait.2$subclass=="Copepoda"]= 1

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$subclass=="Copepoda"]=0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$subclass=="Copepoda"]= 0


  ######
  ##### subclass Phyllopoda #####
  ######

  ### MD1 (none): 0
  trait.2$MD1[trait.2$subclass=="Phyllopoda"]= 0

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$subclass=="Phyllopoda"]= 0

  ### MD3 (hair) (1)
  trait.2$MD3[trait.2$subclass=="Phyllopoda"]= 1

  ### MD4 (sclerotized spines) (1)
  trait.2$MD4[trait.2$subclass=="Phyllopoda"]= 1

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$subclass=="Phyllopoda"]= 0

  ### MD6 (sclerotized exoskeleton) (1)
  trait.2$MD6[trait.2$subclass=="Phyllopoda"]= 1

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$subclass=="Phyllopoda"]= 0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$subclass=="Phyllopoda"]= 0



  ###########################################################

  ##### Ordem Hemiptera  #####

  #########################################################

  ### MD1 (none): 0
  trait.2$MD1[trait.2$ord=="Hemiptera"]= 0

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$ord=="Hemiptera"]= 0

  ### MD3 (hair) (1)
  trait.2$MD3[trait.2$ord=="Hemiptera"]= 1

  ### MD4 (sclerotized spines) (1)
  trait.2$MD4[trait.2$ord=="Hemiptera"]= 1

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$ord=="Hemiptera"]= 0

  ### MD6 (sclerotized exoskeleton) (3)
  trait.2$MD6[trait.2$ord=="Hemiptera"]= 3

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$ord=="Hemiptera"]= 0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$ord=="Hemiptera"]= 0


  ###########################################################

  ##### Ordem Lepidoptera  ##### all larvae (?)

  #########################################################

  ### MD1 (none): 0
  trait.2$MD1[trait.2$ord=="Lepidoptera"]= 0

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$ord=="Lepidoptera"]= 1

  ### MD3 (hair) (1)
  trait.2$MD3[trait.2$ord=="Lepidoptera"]= 2

  ### MD4 (sclerotized spines) (1)
  trait.2$MD4[trait.2$ord=="Lepidoptera"]= 0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$ord=="Lepidoptera"]= 1

  ### MD6 (sclerotized exoskeleton) (1)
  trait.2$MD6[trait.2$ord=="Lepidoptera"]= 0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$ord=="Lepidoptera"]= 0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$ord=="Lepidoptera"]= 0

  ###########################################################

  ##### Family Ephydridae #####

  #########################################################

  # Merrit et al. An introduction to the aquatic insects of north america:... integument of posterior
  # abdominal segments covered with setae or spinules, or with setaceuous (setae-bearing) tubercles on
  # some segments (Ephydridae)

  ### MD1 (none): 0
  trait.2$MD1[trait.2$family=="Ephydridae"]= 0

  ### MD2 (elongated tubercle) (1)
  trait.2$MD2[trait.2$family=="Ephydridae"]= 1

  ### MD3 (hair) (1)
  trait.2$MD3[trait.2$family=="Ephydridae"]= 1

  ### MD4 (sclerotized spines) (1)
  trait.2$MD4[trait.2$family=="Ephydridae"]= 1

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Ephydridae"]= 0

  ### MD6 (sclerotized exoskeleton) (1)
  trait.2$MD6[trait.2$family=="Ephydridae"]= 0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$family=="Ephydridae"]= 0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="Ephydridae"]= 0

  #################################################

  #####   subclass Hirudinea #########

  #################################################


  ### MD1 (none): (3)
  trait.2$MD1[trait.2$subclass=="Hirudinea"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$subclass=="Hirudinea"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$subclass=="Hirudinea"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$subclass=="Hirudinea"]= 0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$subclass=="Hirudinea"]= 0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$subclass=="Hirudinea"]= 0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$subclass=="Hirudinea"]= 0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$subclass=="Hirudinea"]= 0

  ###########################################################

  ##### Family Limoniidae / Tipulidae#####

  #########################################################

  ### MD1 (none): (3)
  trait.2$MD1[trait.2$family=="Limoniidae"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Limoniidae"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Limoniidae"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Limoniidae"]= 0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Limoniidae"]= 0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="Limoniidae"]= 0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$family=="Limoniidae"]= 0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="Limoniidae"]= 0

  ## Tipulidae

  ### MD1 (none): (3)
  trait.2$MD1[trait.2$family=="Tipulidae"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Tipulidae"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Tipulidae"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Tipulidae"]= 0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Tipulidae"]= 0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="Tipulidae"]= 0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$family=="Tipulidae"]= 0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="Tipulidae"]= 0

  ###########################################################

  ##### Family Dixidae#####

  #########################################################

  ### MD1 (none): (3)
  trait.2$MD1[trait.2$family=="Dixidae"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Dixidae"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Dixidae"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Dixidae"]= 0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Dixidae"]= 0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="Dixidae"]= 0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$family=="Dixidae"]= 0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="Dixidae"]= 0


  ###########################################################

  ##### Family Muscidae #####

  #########################################################

  ### MD1 (none): (3)
  trait.2$MD1[trait.2$family=="Muscidae"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Muscidae"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Muscidae"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Muscidae"]= 0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Muscidae"]= 0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="Muscidae"]= 0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$family=="Muscidae"]= 0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="Muscidae"]= 0



  ###################### Subclass Oligochaeta ################################

  ## First I inserted the values that are common for all the oligochaeta

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$subclass=="Oligochaeta"]=0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$subclass=="Oligochaeta"]=0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$subclass=="Oligochaeta"]=0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$subclass=="Oligochaeta"]=0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$subclass=="Oligochaeta"]=0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$subclass=="Oligochaeta"]=0


  ##### Family Aeolosomatidae ######

  #### This Oligochaeta Family has a few setae on the tegument

  ### MD1 (none): (0)
  trait.2$MD1[trait.2$family=="Aeolosomatidae"]=0

  ### MD3 (hair) (1)
  trait.2$MD3[trait.2$family=="Aeolosomatidae"]=1


  ##### Enchytraeoidae - oligochaeta #####

  ### MD1 (none): (3)
  trait.2$MD1[trait.2$family=="Enchytraeoidae"]= 3

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Enchytraeoidae"]= 0


  #### family Naididae

  ### MD1 (none): (0)
  trait.2$MD1[trait.2$family=="Naididae"]= 0

  ### MD3 (hair) (1)
  trait.2$MD3[trait.2$family=="Naididae"]= 1


  #### oligochaeta - family NA (here I inserted the value of the Mode)
  ### MD1 (none): (0)
  trait.2$MD1[trait.2$subclass=="Oligochaeta" & is.na(trait.2$family)]= 0

  ### MD3 (hair) (1)
  trait.2$MD3[trait.2$subclass=="Oligochaeta" & is.na(trait.2$family)]= 1

  ## family Lumbricidae
  ### MD1 (none): (3)
  trait.2$MD1[trait.2$family=="Lumbricidae"]= 3

  ### MD3 (hair) (1)
  trait.2$MD3[trait.2$family=="Lumbricidae"]= 0

  ## family Acanthodrilidae
  ### MD1 (none): (3)
  trait.2$MD1[trait.2$family=="Acanthodrilidae"]= 3

  ### MD3 (hair) (1)
  trait.2$MD3[trait.2$family=="Acanthodrilidae"]= 0



  ### we have one Annelida without information regarding Sub Class

  ### MD1 (none): (0)
  trait.2$MD1[trait.2$phylum=="Annelida" & is.na(trait.2$subclass)]= 0

  ### MD3 (hair) (1)
  trait.2$MD3[trait.2$phylum=="Annelida" & is.na(trait.2$subclass)]= 1

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$phylum=="Annelida" & is.na(trait.2$subclass)]=0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$phylum=="Annelida" & is.na(trait.2$subclass)]=0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$phylum=="Annelida" & is.na(trait.2$subclass)]=0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$phylum=="Annelida" & is.na(trait.2$subclass)]=0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$phylum=="Annelida" & is.na(trait.2$subclass)]=0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$phylum=="Annelida" & is.na(trait.2$subclass)]=0



  #########################################################

  ############## phylum Platyhelminthes #################

  ########################################################

  ### MD1 (none): 3
  trait.2$MD1[trait.2$phylum=="Platyhelminthes"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$phylum=="Platyhelminthes"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$phylum=="Platyhelminthes"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$phylum=="Platyhelminthes"]= 0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$phylum=="Platyhelminthes"]= 0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$phylum=="Platyhelminthes"]= 0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$phylum=="Platyhelminthes"]= 0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$phylum=="Platyhelminthes"]= 0

  #########################################################

  ############## phylum Nematomorpha #################

  ########################################################

  ### MD1 (none): 3
  trait.2$MD1[trait.2$phylum=="Nematomorpha"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$phylum=="Nematomorpha"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$phylum=="Nematomorpha"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$phylum=="Nematomorpha"]= 0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$phylum=="Nematomorpha"]= 0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$phylum=="Nematomorpha"]= 0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$phylum=="Nematomorpha"]= 0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$phylum=="Nematomorpha"]= 0


  ## Nematoda
  ### MD1 (none): 3
  trait.2$MD1[trait.2$phylum=="Nematoda"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$phylum=="Nematoda"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$phylum=="Nematoda"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$phylum=="Nematoda"]= 0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$phylum=="Nematoda"]= 0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$phylum=="Nematoda"]= 0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$phylum=="Nematoda"]= 0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$phylum=="Nematoda"]= 0


  ####################################################

  ##### Family PLanorbidae (Gastropoda)#####

  ##################################################

  ## Nematoda
  ### MD1 (none): (0)
  trait.2$MD1[trait.2$family=="Planorbidae"]= 0

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Planorbidae"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Planorbidae"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Planorbidae"]= 0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Planorbidae"]= 0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="Planorbidae"]= 0

  ### MD7 (Shell) (3)
  trait.2$MD7[trait.2$family=="Planorbidae"]= 3 # one more with shell, but this is terrestrial

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="Planorbidae"]= 0


  ########################################################

  ################## Family Syrphidae ####################

  ########################################################

  ##### common for all syrphidae
  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Syrphidae"]= 0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Syrphidae"]= 0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="Syrphidae"]= 0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$family=="Syrphidae"]= 0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="Syrphidae"]= 0


  #### genus Copestylum

  ### MD1 (none): (0)
  trait.2$MD1[trait.2$genus=="Copestylum"]= 0

  ### MD2 (elongated tubercle) (2)
  trait.2$MD2[trait.2$genus=="Copestylum"]= 2 ### they have small or large lateral projections around the margins of the body

  ### MD3 (hair) (2)
  trait.2$MD3[trait.2$genus=="Copestylum"]= 2 ### they have small dense hairs


  #### genus Eristalis

  ### MD1 (none): (3)
  trait.2$MD1[trait.2$genus=="Eristalis"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$genus=="Eristalis"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$genus=="Eristalis"]= 0

  #### genus Meromacrus
  ### MD1 (none): (3)
  trait.2$MD1[trait.2$genus=="Meromacrus"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$genus=="Meromacrus"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$genus=="Meromacrus"]= 0


  #### genus Palpada
  ### MD1 (none): (3)
  trait.2$MD1[trait.2$genus=="Palpada"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$genus=="Palpada"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$genus=="Palpada"]= 0


  #### genus Quichuana
  ### MD1 (none): (3)
  trait.2$MD1[trait.2$genus=="Quichuana"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$genus=="Quichuana"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$genus=="Quichuana"]= 0

  #### genus Lejops
  ### MD1 (none): (3)
  trait.2$MD1[trait.2$genus=="Lejops"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$genus=="Lejops"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$genus=="Lejops"]= 0


  #### genus Xilota
  ### MD1 (none): (3)
  trait.2$MD1[trait.2$genus=="Xilota"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$genus=="Xilota"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$genus=="Xilota"]= 0

  #### genus Ocyptamus
  ### MD1 (none): (3)
  trait.2$MD1[trait.2$genus=="Ocyptamus"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$genus=="Ocyptamus"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$genus=="Ocyptamus"]= 0

  #### genus Pipiza
  ### MD1 (none): (3)
  trait.2$MD1[trait.2$genus=="Pipiza"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$genus=="Pipiza"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$genus=="Pipiza"]= 0


  #### Shyrohydae family = NA

  ### MD1 (none): (3)
  trait.2$MD1[trait.2$family=="Syrphidae" & is.na(trait.2$genus)]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Syrphidae"& is.na(trait.2$genus)]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Syrphidae"& is.na(trait.2$genus)]= 0


  ########################################################

  ################## Sciaridae ###########################

  ########################################################

  ### MD1 (none): 3
  trait.2$MD1[trait.2$family=="Sciaridae"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Sciaridae"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Sciaridae"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Sciaridae"]= 0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Sciaridae"]= 0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="Sciaridae"]= 0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$family=="Sciaridae"]= 0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="Sciaridae"]= 0


  ########################################################

  ###### family Periscelididae ######

  ########################################################

  ### MD1 (none): (3)
  trait.2$MD1[trait.2$family=="Periscelididae"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Periscelididae"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Periscelididae"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Periscelididae"]= 0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Periscelididae"]= 0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="Periscelididae"]= 0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$family=="Periscelididae"]= 0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="Periscelididae"]= 0


  ########################################################

  ###### family Phoridae ######

  ########################################################

  ### MD1 (none): (3)
  trait.2$MD1[trait.2$family=="Phoridae"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Phoridae"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Phoridae"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Phoridae"]= 0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Phoridae"]= 0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="Phoridae"]= 0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$family=="Phoridae"]= 0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="Phoridae"]= 0


  ########################################################

  ###### family Sphaeroceridae ######

  ########################################################

  ### MD1 (none): (3)
  trait.2$MD1[trait.2$family=="Sphaeroceridae"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Sphaeroceridae"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Sphaeroceridae"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Sphaeroceridae"]= 0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Sphaeroceridae"]= 0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="Sphaeroceridae"]= 0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$family=="Sphaeroceridae"]= 0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="Sphaeroceridae"]= 0


  ########################################################

  ###### family Stratiomyidae ######

  ########################################################

  ### Tachet: Corps aplati dorso-ventralement.

  ### MD1 (none): (0)
  trait.2$MD1[trait.2$family=="Stratiomyidae"]= 0

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Stratiomyidae"]= 0

  ### MD3 (hair) (1)
  trait.2$MD3[trait.2$family=="Stratiomyidae"]= 1

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Stratiomyidae"]= 0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Stratiomyidae"]= 0

  ### MD6 (sclerotized exoskeleton) (2)
  trait.2$MD6[trait.2$family=="Stratiomyidae"]= 2 ### it is not a trully sclerotization (I think), but Tachet: "Tégument de consistance cornée et d'aspect chagriné imprégnés de calcaire."

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$family=="Stratiomyidae"]= 0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="Stratiomyidae"]= 0


  ########################################################

  ###### family Sarchophagidae ######

  ########################################################

  ### MD1 (none): (3)
  trait.2$MD1[trait.2$family=="Sarchophagidae"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Sarchophagidae"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Sarchophagidae"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Sarchophagidae"]= 0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Sarchophagidae"]= 0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="Sarchophagidae"]= 0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$family=="Sarchophagidae"]= 0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="Sarchophagidae"]= 0



  ########################################################

  ################## Tabanidae ###########################

  ########################################################

  ### MD1 (none): (3)
  trait.2$MD1[trait.2$family=="Tabanidae"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Tabanidae"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Tabanidae"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Tabanidae"]= 0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Tabanidae"]= 0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="Tabanidae"]= 0

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$family=="Tabanidae"]= 0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="Tabanidae"]= 0



  ##########################################################################

  ##### Ord Trichoptera  #####

  ###########################################################################
  ### Information from the book: "Costa et al.2006. Insetos Imaturos: metamorfose e identificao. Holos Editora, Riberao Preto"

  ### MD1 (none): 0
  trait.2$MD1[trait.2$ord=="Trichoptera"]= 0

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$ord=="Trichoptera"]=0

  ### MD3 (hair) (1)
  trait.2$MD3[trait.2$ord=="Trichoptera"]=1

  ### MD4 (sclerotized spines) (1)
  trait.2$MD4[trait.2$ord=="Trichoptera"]=1

  ### MD5 (dorsal plates) (1)
  trait.2$MD5[trait.2$ord=="Trichoptera"]=1

  ### MD6 (sclerotized exoskeleton)
  trait.2$MD6[trait.2$ord=="Trichoptera"]=0

  ### MD7 (Shell)(0)
  trait.2$MD7[trait.2$ord=="Trichoptera"]=0

  ### MD8 (case/tube) (3)
  trait.2$MD8[trait.2$ord=="Trichoptera"]=3



  ########################################################

  ### subord Brachycera ###### Family = NA

  ########################################################

  ###  within Brachycera We have organisms without identification (Family).
  ### Since the majority of Brachycera do not have defence (e.g., Dolychopodidae, Empididae, Tabanidae, Muscidae, Phoridae etc)
  ### I coded the Diptera without family identification as no defense (3)

  ### MD1 (none): (3)
  trait.2$MD1[trait.2$subord=="Brachycera" & is.na(trait.2$family)]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$subord=="Brachycera"& is.na(trait.2$family)]=0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$subord=="Brachycera"& is.na(trait.2$family)]=0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$subord=="Brachycera"& is.na(trait.2$family)]=0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$subord=="Brachycera"& is.na(trait.2$family)]=0

  ### MD6 (sclerotized exoskeleton)
  trait.2$MD6[trait.2$subord=="Brachycera"& is.na(trait.2$family)]=0

  ### MD7 (Shell)(0)
  trait.2$MD7[trait.2$subord=="Brachycera"& is.na(trait.2$family)]=0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$subord=="Brachycera"& is.na(trait.2$family)]=0


  ##### For the Diptera without suborder identification I will also code them as 3 for no defence;

  ### MD1 (none): (3)
  trait.2$MD1[trait.2$ord=="Diptera" & is.na(trait.2$subord)]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$ord=="Diptera" & is.na(trait.2$subord)]=0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$ord=="Diptera" & is.na(trait.2$subord)]=0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$ord=="Diptera" & is.na(trait.2$subord)]=0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$ord=="Diptera" & is.na(trait.2$subord)]=0

  ### MD6 (sclerotized exoskeleton)
  trait.2$MD6[trait.2$ord=="Diptera" & is.na(trait.2$subord)]=0

  ### MD7 (Shell)(0)
  trait.2$MD7[trait.2$ord=="Diptera" & is.na(trait.2$subord)]=0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$ord=="Diptera" & is.na(trait.2$subord)]=0


  ########################################################

  ### Family Scatopsidae ######

  ########################################################


  # Smith 1989. The two posterior spiracles each on the tip of a cylindrical process (except Eclaetia fig . lOO).
  # Larvae usually dark or at least brown or yellow and rather hairy; very inactive; in dung,
  # soil or in drier habitats with decaying material, e.g. birds' nests, tree holes (figs 92 100)

  ### MD1 (none): (0)
  trait.2$MD1[trait.2$family=="Scatopsidae"]= 0

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Scatopsidae"]=0

  ### MD3 (hair) (3)
  trait.2$MD3[trait.2$family=="Scatopsidae"]=3

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Scatopsidae"]=0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Scatopsidae"]=0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="Scatopsidae"]=0

  ### MD7 (Shell)(0)
  trait.2$MD7[trait.2$family=="Scatopsidae"]=0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="Scatopsidae"]=0


  ########################################################

  ### Family Thaumaleidae ######

  ########################################################

  ### MD1 (none): (3)
  trait.2$MD1[trait.2$family=="Thaumaleidae"]= 3

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$family=="Thaumaleidae"]=0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$family=="Thaumaleidae"]=0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$family=="Thaumaleidae"]=0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$family=="Thaumaleidae"]=0

  ### MD6 (sclerotized exoskeleton) (0)
  trait.2$MD6[trait.2$family=="Thaumaleidae"]=0

  ### MD7 (Shell)(0)
  trait.2$MD7[trait.2$family=="Thaumaleidae"]=0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$family=="Thaumaleidae"]=0


  ###########################################################

  ##### Ordem Ephemeroptera  #####

  #########################################################

  ### MD1 (none): (0)
  trait.2$MD1[trait.2$ord=="Ephemeroptera"]= 0

  ### MD2 (elongated tubercle) (0)
  trait.2$MD2[trait.2$ord=="Ephemeroptera"]= 0

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$ord=="Ephemeroptera"]= 0

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$ord=="Ephemeroptera"]= 0

  ### MD5 (dorsal plates) (0)
  trait.2$MD5[trait.2$ord=="Ephemeroptera"]= 0

  ### MD6 (sclerotized exoskeleton) (2)
  trait.2$MD6[trait.2$ord=="Ephemeroptera"]= 2

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$ord=="Ephemeroptera"]= 0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$ord=="Ephemeroptera"]= 0

  ###########################################################

  ##### Ordem Megaloptera  #####

  #########################################################

  ### MD1 (none): (0)
  trait.2$MD1[trait.2$ord=="Megaloptera"]= 0

  ### MD2 (elongated tubercle) (3)
  trait.2$MD2[trait.2$ord=="Megaloptera"]= 3

  ### MD3 (hair) (0)
  trait.2$MD3[trait.2$ord=="Megaloptera"]= 1

  ### MD4 (sclerotized spines) (0)
  trait.2$MD4[trait.2$ord=="Megaloptera"]= 0

  ### MD5 (dorsal plates) (1)
  trait.2$MD5[trait.2$ord=="Megaloptera"]= 1 # 3 segments with dorsal plates

  ### MD6 (sclerotized exoskeleton) (2)
  trait.2$MD6[trait.2$ord=="Megaloptera"]= 2

  ### MD7 (Shell) (0)
  trait.2$MD7[trait.2$ord=="Megaloptera"]= 0

  ### MD8 (case/tube) (0)
  trait.2$MD8[trait.2$ord=="Megaloptera"]= 0


  stopifnot(nrow(trait.1) == nrow(trait.2))

  return(trait.2)
}
# write.csv(trait.3, "data-intermediate/11MD_trait.csv", row.names = FALSE)

