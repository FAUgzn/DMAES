# Basic code for the Phanerozoic diversity handout
#
#   # 1. I used this link to download the data, saved it as 'all_2018-09-14.csv'
#	https://paleobiodb.org/data1.2/occs/list.csv?datainfo&rowcount&interval=Ediacaran,Holocene&show=class,classext,genus,subgenus,abund,coll,coords,loc,paleoloc,strat,stratext,lith,env,ref,crmod,timebins,timecompare
#	
#   # 2. Write binary file, so that it takes up less space:
#	dat <- read.csv("D:/all_2018-10-30.csv", header=T, 
#		stringsAsFactors=F)
#
#		# make this dataset somewhat slimmer so that it can be hosted on github
#		need<-c(
#			"collection_no",
#			"collection_name",
#			"identified_rank",
#			"identified_name",
#			"accepted_name",
#			"accepted_rank",
#			"early_interval",
#			"late_interval",
#			"max_ma",
#			"min_ma",
#			"reference_no",
#			"phylum",
#			"class",
#			"order",
#			"family",
#			"genus",
#			"lng",
#			"lat",
#			"paleolng",
#			"paleolat",
#			"formation",
#			"lithology1",
#			"lithification1",
#			"environment",
#			"created",
#			"zone", 
#			"stratcomments"
#		)
#
#		dat <- dat[,need]
#
#	save(dat, file="data/PaleoDB/2018-10-30_paleoDB.RData")
#	# then it was posted on GitHub
#
################################################################################
# workflow starts from here, set your working diretory!!!
	workdir<-"D:/Dropbox/WorkSpace/- Teaching/2018-10-30_DMAES/"
#	workdir <- "/media/kocsis/Data/Dropbox/WorkSpace/- Teaching/2018-10-30_DMAES"
	setwd(workdir)

	# load the package
	library(divDyn)

################################################################################
# read the data table
	load(url("https://github.com/FAUgzn/DMAES/raw/master/data/PaleoDB/2018-10-30_paleoDB.RData"))

# 1. taxonomic filtering
	# filter records not identified at least to genus
	dat <-dat[dat$accepted_rank %in% c("genus", "species"),]

	#A. phyla
	marineNoPlant <- c("",
		"Agmata",
		"Annelida",
		"Bilateralomorpha",
		"Brachiopoda",
		"Bryozoa",
		"Calcispongea",
		"Chaetognatha",
		"Cnidaria",
		"Ctenophora",
		"Echinodermata",
		"Entoprocta",
		"Foraminifera",
		"Hemichordata",
		"Hyolitha",
		"Mollusca",
		"Nematoda",
		"Nematomorpha",
		"Nemertina",
		"Onychophora",
		"Petalonamae",
		"Phoronida",
		"Platyhelminthes",
		"Porifera",
		"Problematica",
		"Rhizopodea",
		"Rotifera",
		"Sarcomastigophora",
		"Sipuncula",
		"Uncertain",
		"Vetulicolia",
		""
	)

	# which rows?
	bByPhyla <- dat$phylum%in% marineNoPlant

	# the other
		noNeed <- dat[!bByPhyla,]
		needPhylum <- dat[bByPhyla,]

	#B. classes
	#	levels(factor(noNeed$class))
		needClass <- c(
			"Acanthodii",
			"Actinopteri",
			"Actinopterygii",
			"Agnatha",
			"Cephalaspidomorphi",
			"Chondrichthyes",
			"Cladistia",
			"Coelacanthimorpha",
			"Conodonta",
			"Galeaspida",
			"Myxini",
			"Osteichthyes",
			"Petromyzontida",
			"Plagiostomi",
			"Pteraspidomorphi",
			# here come the Arthropods
			"Artiopoda",
			"Branchiopoda",
			"Cephalocarida",
			"Copepoda",
			"Malacostraca",
			"Maxillopoda",
			"Megacheira",
			"Merostomoidea",
			"Ostracoda",
			"Paratrilobita",
			"Pycnogonida",
			"Remipedia",
			"Thylacocephala",
			"Trilobita",
			"Xiphosura"
		)
		
		# which rows?
		bNeedClass <- dat$class %in% needClass

	#C.  mammals
	#	mammals <- dat[dat$class=="Mammalia",]
	#	levels(factor(mammals$order))

		needMammalOrd <- c("Cetacea", "Sirenia")

		# which rows?
		bMammalOrder <- dat$order %in% needMammalOrd

		# the carnivores
		carnivores <- dat[dat$order=="Carnivora",]
		levels(factor(carnivores$family))

		needFam <- c("Otariidae", "Phocidae", "Desmatophocidae")

		# which rows?
		bNeedMamFam<- dat$family%in%needFam

	# D. Reptiles
	#	reptiles <- dat[dat$class=="Reptilia",]
	#	levels(factor(reptiles$order))

		needReptOrd<-c(
			"Eosauropterygia",
			"Hupehsuchia",
			"Ichthyosauria",
			"Placodontia",
			"Sauropterygia",
			"Thalattosauria"
		)
		
		# which rows?
		bRept <- dat$order%in%needReptOrd

	# E. turtles 
	#	turtles <- dat[dat$order=="Testudines",]
	#	levels(factor(turtles$family))
	
	# Chelonioidea turtles
	needTurtleFam <- c(
		"Cheloniidae",
		"Protostegidae",
		"Dermochelyidae",
		"Dermochelyoidae",
		"Toxochelyidae",
		"Pancheloniidae"
	)

	# which rows?
	bTurtle <- dat$family%in%needTurtleFam

	# subset the data
	dat <- dat[
		bByPhyla |
		bNeedClass |
		bMammalOrder |
		bNeedMamFam |
		bRept | 
		bTurtle
		, ]


	# resolve the potential homonymy problem
	dat$clgen <- paste(dat$class, dat$genus)

################################################################################
# 2. filter by environment
	levels(factor((dat$environment)))

	omitEnv <- c(
		"\"floodplain\"",
		"alluvial fan",
		"cave",
		"\"channel\"",
		"channel lag" ,
		"coarse channel fill",
		"crater lake",
		"crevasse splay",
		"dry floodplain",
		"delta plain",
		"dune",
		"eolian indet.",
		"fine channel fill",
		"fissure fill",
		"fluvial indet.",
		"fluvial-lacustrine indet.",
		"fluvial-deltaic indet.",
		"glacial",
		"interdune",
		"karst indet.",
		"lacustrine - large",
		"lacustrine - small",
		"lacustrine delta front",
		"lacustrine delta plain",
		"lacustrine deltaic indet.",
		"lacustrine indet.",
		"lacustrine interdistributary bay",
		"lacustrine prodelta",
		"levee",
		"loess",
		"mire/swamp",
		"pond",
		"sinkhole",
		"spring",
		"tar",
		"terrestrial indet.",
		"wet floodplain"
	)

	dat<-dat[!dat$environment%in%omitEnv, ]


# finally omit unlithified sediments
	dat <- dat[dat$lithification1!="unlithified",]



# save(dat, file="data/PaleoDB/filtered_2018-10-30.RData")









################################################################################
# 3. for stage-level assignments!
	# time scales
	data(stages)
		
	# load lookup keys
	data(keys)

# do the same for the stages
# B. the stg entries (lookup)
		stgMin<-categorize(dat[,"early_interval"],keys$stgInt)
		stgMax<-categorize(dat[,"late_interval"],keys$stgInt)

		# convert to numeric
		stgMin<-as.numeric(stgMin)
		stgMax<-as.numeric(stgMax)

	# empty container
		dat$stg <- rep(NA, nrow(dat))

	# select entries, where
		stgCondition <- c(
		# the early and late interval fields indicate the same stg
			which(stgMax==stgMin),
		# or the late_intervarl field is empty
			which(stgMax==-1))

	# in these entries, use the stg indicated by the early_interval
		dat$stg[stgCondition] <- stgMin[stgCondition]

	# terrible in the pre-Silurian
	# additional treatment required for Cambrian
		# load data
		load(url("https://github.com/adamkocsis/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/cambStrat.RData"))
		# correct it with this function
		source("https://github.com/adamkocsis/ddPhanero/raw/master/scripts/strat/2018-08-31/cambProcess.R")

	# and the Ordovician
		# load data
		load(url("https://github.com/adamkocsis/ddPhanero/raw/master/data/Stratigraphy/2018-08-31/ordStrat.RData"))
		# correct it with this function
		source("https://github.com/adamkocsis/ddPhanero/raw/master/scripts/strat/2018-08-31/ordProcess.R")

