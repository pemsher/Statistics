# Pemba Sherpa
# Unusual Options activity tracker
# This contains methods scans for option series with volume > open interest
# When it finds option series with volume > OI, it then scans the option time and sale
# to find large block orders
# ==============================================
# usage:  symbols : array of underlying or "dow" / "nasdaq"
#         expiration: YYYY-MM-DD format options expiration
#         seriesVolume: default 1000, volume for the option series
#         saleVolume: default 500, block volume for each sale transaction of the option series
# Note:
# The timesales api gives aggregate of volume transaction based on minute as the smallest unit
# This can give some false positive for large orders
# Once you have this information, I recommend using your brokerage software which can give you more granular
# time and sale data to identify block orders
library(corrplot)
library(stringr)
library(httr)
library(jsonlite)
library(stats)


getMarketData <- function(url)
{
  print(url)
	marketData <- tryCatch(
        	{
    	   		fromJSON(content(GET(url=url,add_headers(Authorization="Bearer i6jVdhOxuVDHAxn88AOajVuvZ3YE",'Content-Type'="application/json")),"text"),flatten=FALSE)
        	}, 
    		error=function(e)
    		{
    		 print(e);
    		 print("Errored...retrying after 60 seconds")
	         Sys.sleep(2)
	         return (fromJSON(content(GET(url=url,add_headers(Authorization="Bearer i6jVdhOxuVDHAxn88AOajVuvZ3YE",'Content-Type'="application/json")),"text"),flatten=FALSE))
			}
    		)
}

getURL <- function(symbol, END_POINT_ASK, expiration )
{
  END_POINTS_KEY <- c("UNDERLYING_END_POINT", "OPTIONS_CHAINS_API_END_POINT", "TIMESALES_API_END_POINT")
  END_POINTS_VALUE <- c("https://sandbox.tradier.com/v1/markets/quotes?", "https://sandbox.tradier.com/v1/markets/options/chains?", "https://sandbox.tradier.com/v1/markets/timesales?")

  END_POINT <- setNames(as.list(END_POINTS_VALUE), END_POINTS_KEY)

  URL <- "";
  if(END_POINT_ASK == "UNDERLYING_END_POINT")
  {
  	URL <- paste(END_POINT[END_POINT_ASK],"symbols=",str_trim(symbol,side="both"),sep="")
  }

  if(END_POINT_ASK == "OPTIONS_CHAINS_API_END_POINT")
  {
  	URL <- paste(END_POINT[END_POINT_ASK],"symbol=",str_trim(symbol,side="both"),"&expiration=",str_trim(expiration,side="both"),sep="")
  }
  
  if(END_POINT_ASK == "TIMESALES_API_END_POINT")
  {
  	URL <- paste(END_POINT[END_POINT_ASK],"symbol=",str_trim(symbol,side="both"),sep="")
  }
  URL
}

getOptionChain <- function(symbols,expiration,seriesVolume=1000,saleVolume=500){

  FILE_NAME = paste(str_trim("C:/pemsher/Statistics/scan_",side="both"),str_trim(Sys.Date(),side="both"),"_",str_trim(format(Sys.time(), "%H-%M-%OS3"),side="both"),sep="")
   
  sink(FILE_NAME,append = FALSE,split = TRUE)
  
  SPY_100 <- c("AAPL","ABBV","ABT","ACN","AGN","AIG","ALL","AMGN","AMZN","AXP","BA","BAC","BIIB","BK","BKNG","BLK","BMY","BRK.B","C","CAT","CELG","CHTR","CL","CMCSA","COF","COP","COST","CSCO","CVS","CVX","DHR","DIS","DUK","DWDP","EMR","EXC","F","FB","FDX","FOX","FOXA","GD","GE","GILD","GM","OOG","GOOGL","GS","HAL","HD","HON","IBM","INTC","JNJ","JPM","KHC","KMI","KO","LLY","LMT","LOW","MA","MCD","MDLZ","MDT","MET","MMM","MO","MN","MRK","MS","MSFT","NEE","NKE","ORCL","OXY","PEP","PFE","PG","PM","PYPL","QCOM","RTN","SBU","SLB","SO","SPG","T","TGT","TWX","TXN","UNH","UNP","UPS","USB","UTX","V","VZ","WBA","WFC","WMT","XOM")

  DOW_30 <- c("AAPL","CSCO","BA","GE","MSFT","JPM","IBM","INTC","WMT","JNJ","NKE","KO","CVX","UNH","UTX","MCD","PG","DIS","TRV","XOM","VZ","MMM","MRK","V","DWDP","HD","PFE","GS","CAT","AXP")

  NASDAQ_30 <- c("FRSH","OPTT","WLB","JIVE","EXAC","NVCR","SLAB","KLXI","NRCIA","AKTX","FNLC","SCON","EXAS","IBKC","FARM","ATEC","FARO","TSLA","CVCY","CVCO","FNJN","CMRX","NRCIB","FEYE","POPE","LTEA","SPHS","FRTA","AGLE","NVCN")

  NASDAQ_100 <- c("AAL","AAPL","ADBE","ADI","ADP","ADSK","AKAM","ALGN","ALXN","AMAT","AMGN","AMZN","ATVI","AVGO","BIDU","BIIB","BMRN","CA","CELG","CERN","CHKP","CHTR","CTRP","CTAS","CSCO","CTXS","CMCSA","COST","CSX","CTSH","DISCA","DISCK","UNCH","DISH","DLTR","EA","EBAY","ESRX","EXPE","FAST","FB","FISV","FOX","FOXA","GILD","GOOG","GOOGL","HAS","HSIC","HOLX","ILMN","INCY","INTC","INTU","ISRG","JBHT","JD","KLAC","KHC","LBTYK","LILA","LBTYA","QRTEA","MELI","MAR","MAT","MDLZ","MNST","MSFT","MU","MXIM","MYL","NCLH","NFLX","NTES","NVDA","PAYX","BKNG","PYPL","QCOM","REGN","ROST","SHPG","SIRI","SWKS","SBUX","SYMC","TSCO","TXN","TMUS","ULTA","VIAB","VOD","VRTX","WBA","WDC","XRAY","IDXX","LILAK","LRCX","MCHP","ORLY","PCAR","STX","TSLA","VRSK","WYNN","XLNX")

  OPTIONABLE <- c("BANR","CMO","ERN","IMO","PFS","REED","SAFE","UONEK","BNCL","CLAR","CUI","DEI","FCF","FMBI","GZT","IMH","IPCI","IRET","OFIX","SPIL","TAT","TRST","XRM","DSS","DTEA","SR","PSB","CKH","FCSC","GLAD","GXP","HBIO","IBCP","MYE","NMRK","ONB","PHH","TRCO","CRESY","HBP","ECOL","FDUS","FET","HUBG","RECN","TRNO","CDR","FBNC","LTXB","MGEE","RNWK","LODE","RGA","REXR","MMSI","AHT","ISCA","JWA","LQ","STCN","VIVE","LION","DLX","HTH","ASR","MATW","OFC","COWN","RSO","ASC","BSQR","CXDC","HAFC","LFGR","ONP","PLOW","SYKE","WABC","ECOM","KOF","BFR","LBRDA","TG","THRM","BMI","OVAS","RCKT","VR","KELYA","AGM","HSTM","MFG","BSET","CNMD","DAKT","ALLT","MGIC","CASH","OGS","NEO","TVTY","PNM","STKL","BRKL","AHH","ATNI","CPSI","CAAS","EMKR","EVTC","FF","HURN","LBRDK","NMR","RTIX","SNMX","SXE","TFSL","UGP","VCYT","CIEIQ","PDM","PNNT","DCO","DRH","RSYS","CIR","SMFG","TKC","NCS","HSII","TLP","AIRG","EPM","NTWK","RM","CXRX","INGN","KONA","CHE","AVX","TBI","B","NNI","MSTR","NPO","RBBN","SMSI","TMQ","UTI","SIGI","XHR","HRTG","TREC","QGEN","AZZ","NSTG","LQDT","JE","FUL","KAMN","DXLG","WAFD","EBR","CBD","EPE","MTU","RCI","SPNE","CSU","STAR","ANGI","POL","STC","FARO","CIA","ABR","LEJU","WLTW","ISIG","HI","VJET","ACH","HR","EFOI","AP","UMC","HALL","RDY","TRIB","WLKP","HYH","BAS","SLRC","IEX","FI","ITGR","HTA","NYLDA","TILE","UUUU","PHX","EHC","CENTA","INN","VOXX","EXLS","LRAD","OMN","PKD","PRMW","SNI","USM","PERI","SXT","UGI","AKG","SKM","ARDX","LFUS","FMS","EQC","CAMT","TRC","ACCO","CWCO","CLGX","FBC","HHS","NWE","NWSA","ORN","ATTU","SSD","IPXL","TYPE","CUB","STAA","WWW","BDN","USNA","OOMA","CHKR","CCRN","FELP","FVE","MRTN","NOA","TU","VNCE","CMD","NGS","ABAX","CHDN","LNTH","TNP","ORIT","CSV","MCRB","POOL","SRDX","SON","WTS","INWK","KEX","CTRE","WRI","ANDE","BCLI","PFSW","FNGN","AVD","CADE","FCBC","AGFS","VREX","RICK","PRIM","HNP","PRK","ENZ","PBT","PCYG","AXU","ITG","XSPA","INT","RMAX","PBRA","CBRE","TSU","HAWK","MVO","HAE","BKCC","NERV","FSTR","BRKR","NGVC","DX","SHOS","CQH","WGP","CSII","WBS","FORR","SOL","TNDM","GTY","WIRE","VLP","LIVN","CLH","LTM","PEO","ESE","GLT","NCI","GATX","PFMT","ZFGN","IIIN","WG","PRSC","NSP","GNMX","PER","TICC","DENN","NNBR","BCOV","BHE","CAFD","AWI","CPL","HZO","PDFS","UTSI","ENS","SAH","PEB","SHOO","NFG","CBB","DWCH","XCOOQ","LRN","CHH","FOMX","PSMT","SPNS","ROIC","SDT","MTRN","ABG","AGRO","SCHL","FIX","LNDC","UPLD","MPAA","RPAI","TPRE","ERA","CCXI","HWCC","CRMD","TTC","SMP","GRC","TROV","NWPX","PNFP","LII","WLB","CIO","AIV","TNK","SLF","NSA","SLGN","NH","NNN","BOOM","EDR","ZX","FTEK","VCRA","SMTX","UFI","TMHC","ATU","ARMK","AVXL","APAM","MDU","AUTO","KALU","AEGN","FSS","AGTC","MDSO","HLIT","CCIH","CSGS","MEIP","LAYN","CBSA","SDR","MATR","FLWS","INVE","LORL","UFPI","SYRS","AMBR","HIFR","MDCA","EFC","CGA","SKIS","RLI","CTLT","BIO","TRIL","RAS","NVLN","RDWR","CEVA","EV","JONE","LPT","BKU","RUSHA","NAO","JMBA","OGE","GRBK","GEN","ATGE","TTEK","OIS","CASC","IBKC","UMPQ","BBOX","FWONK","UVE","HELE","HRG","CODI","EHTH","DRD","VICI","CPN","WWD","MNTX","XOXO","MEI","HYGS","IBP","OCSL","KRNT","OMF","PZE","PES","SATS","DRE","VPG","WTFC","MTL","HMSY","MUSA","NXRT","TZOO","NEU","SBS","RP","FOE","CPT","AZUL","RPT","MLHR","BAP","KAI","LPNT","MBFI","SBSI","INOV","HRC","KNL","TNAV","AIN","TERP","LXP","CRME","APT","INSG","KOP","HT","LAUR","RYI","PSO","NLS","FOX","AFMD","SREV","OCIP","EWBC","NICE","NAVB","ACET","NHC","ATO","NR","FWONA","ELP","VIVO","SKYW","ORAN","AMH","MKTX","MTGE","FLY","EGLT","MITT","MTRX","DBVT","VIV","AVT","WCC","HCKT","BKEP","ZIXI","GAIN","APDN","LILAK","BPI","ATEN","BDC","NHTC","MPWR","WD","ARCT","GEOS","AMRC","BIOS","AMED","AXE","CDW","RRTS","TCPC","PLXS","CCO","PVAC","TRS","VSAT","CNTY","EBS","SPXC","STB","ENT","WMS","BCPC","BAK","AVHI","CUR","TEO","DLNG","ASX","XENT","ENLC","PCSB","WSR","KDMN","GPMT","RNR","CAJ","RES","BOKF","SRC","MTX","SNN","BABY","CNIT","DCI","NM","TOWR","IMPV","HIW","BBW","MODN","BXS","VLRS","TCO","HY","SJR","ITRI","TREX","LZB","BFAM","MATX","FDP","PBPB","KN","AIR","FBP","GASS","FAF","TWI","VTL","SCSC","FCFS","MWA","FPI","RGLS","SANW","ENVA","GPK","INAP","IVAC","AZPN","VVC","UIHC","EE","NRP","GSAT","CNET","GFF","ANFI","SSL","ARDM","ICLK","MBUU","AVA","VSTO","TCRD","LPCN","KRG","ST","MRC","ECPG","SHLM","GV","GOOD","TTMI","GTS","AKBA","CORR","RBA","MSGN","NFBK","OSUR","OPHT","BFB","AUDC","CRH","TOUR","MKL","AWR","COL","LYTS","CLI","GOGL","KEP","AMR","XOG","SUNW","DPW","INTL","KS","PBA","LECO","HMHC","MG","NANO","CPLA","MHLD","CBU","CYTX","CSLT","OSIS","EVR","INST","DOX","KYN","ALGT","COLM","POR","CRK","COMM","MFA","EGL","PTR","TSQ","CTIC","PATK","ANSS","PRA","MHO","CTSO","GEVO","VAR","REDU","RYAAY","ARTX","ROCK","MN","GBCI","BAH","BOOT","SF","WEC","VGZ","RXN","LSXMA","DIOD","PGTI","XPER","CYRX","SHEN","BRG","HIVE","WK","GTE","NOVT","MGLN","BSM","PJC","PLX","EMMS","NEWM","ES","NVMI","ORA","ESS","SALT","PGEM","CYD","WAIR","RAIL","VC","PDCE","TRVN","CXP","BMCH","WAT","CUBE","WSO","PKI","WNC","SIR","DLB","HRI","GMED","SCWX","CWST","BMA","AM","PRFT","PRKR","AAU","MCRN","DRQ","RAVN","HHC","GWR","KT","FTD","CBPX","BGC","SNH","BCOR","HPR","CMRX","PRI","BPOP","NTRA","LXU","JELD","UDR","VSI","CHFC","BERY","PUK","KIRK","SIFY","XNCR","FLIR","BJRI","LSCC","ARGS","MXL","LXFT","RPXC","LNN","ARCB","G","BWXT","TRMB","RHP","NUVA","ALOG","KEYW","CLD","CW","EPZM","VDSI","SERV","CHT","MOBL","ESRT","KLXI","FRGI","HBHC","XBIT","CCMP","LGFB","LBTYK","PACB","WLH","FSIC","ROLL","KAR","ARE","KOS","AGRX","LN","SANM","ORBC","CWT","MTH","GIB","WBC","VMI","SEAC","EDGE","ANIK","WOR","UNT","AVID","MCF","WAB","SE","MACK","LX","FCB","NX","EGBN","ILPT","CRAY","PMT","ALT","REG","DTE","NTGR","PJT","CVEO","DSL","TCF","WMC","CDMO","BRX","NDSN","ALLE","ELGX","CMS","ABLX","ORI","ITT","HXL","MMLP","STO","WRE","ARI","EZPW","TDW","CFR","ISBC","PRLB","LHO","PSXP","UBSH","PNK","RHI","RESN","BBRG","CECE","GNTX","QUAD","EGRX","MNTA","TXRH","ULTI","MORN","UVV","KBAL","GLDD","VECO","EVEP","PBCT","SFUN","ASB","UAN","CZZ","HOLI","TSE","EQGP","SUM","MD","GLOP","MSM","FRSH","KND","VRS","SXC","WIT","TVPT","NYMT","MLNT","GLPI","AVH","BGG","PSDV","TUES","QRTEA","AYR","BECN","LSTR","TRNC","GHL","ACIW","LVNTA","MITL","WTR","MCRI","TGB","LOB","JAKK","SPSC","RVNC","IDSY","ALV","CRS","BRO","IPHI","OXM","TCBI","ASA","CMCO","CUTR","STE","EYES","CALL","MYRG","AAC","INXN","FRAC","CPS","LEE","ACOR","AIZ","WCG","MRCY","MCC","JBGS","HEP","MBBYF","CVG","PINC","OTEX","ZEUS","CGIX","TYL","GORO","IDRA","CAPL","STRL","CPLP","USAT","AJRD","PARR","UEC","EGAN","WIN","HTLD","GCO","MAN","THS","OBE","GVA","CLDT","VG","CHD","CDZI","SPWH","XEL","BSBR","WERN","MAG","ROP","NK","AMN","TRU","CYTK","DNLI","SID","CHUY","ADSW","DORM","AAN","RUTH","WPC","VAC","CNO","CNXM","CYTR","RWT","ROG","VICL","RPD","LKSD","SEM","SPA","NVEE","SECO","QUOT","VNOM","WPP","CBSH","SEED","MYOK","GOLF","TPH","NTP","PHG","IQV","SLS","ETH","FICO","ASTE","KRA","MANU","AB","HOS","ACC","HOMB","PERY","CRMT","NQ","ENBL","PAM","CACC","ABUS","NMIH","VVUS","FLDM","H","GIL","PCOM","CSTE","ATR","DFIN","LLNW","TEP","REGI","MDC","AAT","PEIX","DEA","LPLA","GDS","FN","PQG","JCOM","SLAB","MSCI","BDSI","JLL","EIGI","HCI","INSM","STAY","SIMO","NOAH","TOO","VERU","MGPI","TRI","FBIO","PETX","FIVN","RGEN","AREX","ASYS","MSB","AMG","LTRPA","SJT","CE","BLD","DS","CYS","PRAH","PKX","RARE","CPRX","RDI","PFGC","SDLP","LINU","MFC","FHB","BBL","CNSL","AMOT","ABEV","QSII","PAG","VYGR","KALA","ALSK","CAL","NGG","GST","OII","VRSK","QVCA","BAM","CINF","CNCE","MOH","CRCM","SEND","CDEV","AROC","RTEC","GCI","MGI","ARW","HMC","VOC","FLOW","SRT","BSMX","QTWO","MMYT","MAA","SSP","EDAP","GLP","IFF","TEGP","RST","SNR","TLGT","ENLK","TCX","TRP","BCE","APOG","NEOS","ADXS","AXGN","ONVO","CALX","LBY","ECR","FUN","HEI","SAIC","TA","TITN","LXRX","SYNT","AVY","BMS","CACI","QADA","BRKS","GLRE","SPH","VET","CALD","DYN","OR","CRL","QTM","ORC","CLIR","WEB","VVV","DXPE","LOPE","SIGM","ANIP","RYN","L","SPHS","APEI","JOE","WIFI","DOC","ARR","BLKB","APU","CNK","SWM","ASPS","GLOG","MANT","EPR","LDOS","KNOP","LKM","KPTI","ARCO","MB","SSC","CCC","MDRX","ASGN","ACTG","LMRK","OUT","ATH","QLYS","IRT","GPI","CATM","AXAS","GNRC","BCC","GGAL","GHDX","TCP","CVLT","TDG","QUIK","MTB","SXCP","GDOT","BXMT","CDK","SWCH","AT","REXX","FRPT","LPSN","NEP","AAV","NAVI","CRNT","TX","ARII","PTC","STON","VNTR","WES","TDS","SCVL","RPM","IRWD","FTAI","TWO","CHU","ELY","BIP","LKQ","DST","STAG","TTEC","EGN","DFRG","DNB","ADUS","MNRO","PEI","SUPN","MFIN","SEIC","SUI","CTG","UFS","FRED","ECHO","SB","AINV","DPLO","HEES","SCS","BL","STRA","HASI","AERI","WAL","MED","BMO","GEO","GIFI","BCRX","SCHN","EGHT","CAF","NXTD","VKTX","MERC","RUBI","OA","SBH","VRNT","PLT","CMPR","CCLP","FNF","OPY","USFD","BIVV","DSKE","BBVA","ARQL","AXTI","CARB","CEQP","VNET","UL","ENR","BVN","AAWW","AEE","FLS","SFL","DNOW","SITE","CM","DAR","DKL","HSC","OCUL","SMCI","TPC","DDR","BRS","CCUR","WSBF","EPAM","CAI","TLYS","BXP","XYL","DEPO","LCII","SMLP","IVR","AL","TMK","VSLR","NEPT","SBNY","RESI","LNCE","VGR","TGNA","RENN","MX","RBC","BOJA","SYNC","MXWL","PODD","PBFX","LM","OREX","CFX","AUO","TPIC","MDP","OTIC","RLGY","ALSN","CLLS","CNI","EPC","FPRX","AHL","MVIS","VSTM","I","SABR","BNS","ELF","UTHR","HLX","COO","AMID","BCO","HBM","CAVM","TTPH","SNPS","MOV","GPT","BKD","USAC","VSH","BNFT","FBHS","FLKS","COLL","NYMX","AXON","BANC","SNDR","RVLT","TEN","IRTC","ENTA","AI","DY","BTE","NVRO","ACM","TRQ","WMGI","ASIX","CPA","NSH","VER","SLG","XONE","OMED","OZRK","AGIO","CPG","INGR","WRD","ITCI","ZNH","NI","XRF","STNG","CEO","BID","EXPD","FORM","FRT","CTRN","CALA","CCS","RJF","TTS","APTI","SIX","REI","IIVI","LGND","SP","VRTV","QIWI","LAD","SWIR","TV","KLIC","UEPS","GFI","SBLK","RMD","AGI","CNP","ARCH","TACO","WAGE","ELLI","PGH","ZUMZ","HDP","MITK","ERIC","PKG","XCRA","HESM","CGI","SMRT","SAFM","FLXN","REV","SPAR","CIG","AGCO","ALDR","TCS","CRBP","AEG","TCAP","IT","COGT","RLJ","GPN","PXLW","ERJ","FHN","AMCN","DCM","BC","ROYT","TWOU","CVA","NVGS","CRIS","FDS","MPW","ADNT","GTLS","SRI","MHK","AME","CONE","ENTG","ODFL","EVA","PACW","LOMA","TOCA","CHUBK","JOBS","AMP","BGNE","FLO","PICO","IOVA","DCP","NTCT","BGFV","PRO","SNV","OZM","ALKS","CPSS","LAZ","LAMR","JBHT","CMTL","MCO","CVGW","FIS","VNDA","PDS","PRAA","NEWR","TM","R","BTX","HK","GEF","ARLP","SNP","YEXT","WU","SAM","JNP","XIN","MAIN","MLNX","PRTK","EPAY","QNST","CASY","COHU","BREW","AWK","TDC","HII","FTV","APH","CRI","GOV","NMM","TGP","WPRT","EC","HCC","ING","XLRN","FGP","ATRS","SCI","CAMP","AMSC","ERII","SNSS","RE","JEC","IRDM","SSTK","APLP","CMC","LOGM","CECO","BR","VEDL","RGS","SEP","E","STWD","ATSG","MTW","AMCX","HQY","TUP","PAGP","SBGL","LLL","HDB","OLLI","RRD","INVH","RTRX","SBRA","ONCE","CVBF","CIT","SLM","JNCE","CDNS","GRA","CXW","VNO","LUK","CCE","ESNT","AYI","SGRY","HDSN","DM","MANH","NDLS","XRAY","EXP","ADMP","SRLP","NXST","NTRS","NLSN","AY","SRCL","CDXC","SRE","SAP","FBR","ACGL","HSIC","RMBS","WBAI","KOPN","BLCM","CERS","GCAP","IDXX","MASI","NAT","VICR","NSU","EQM","PTX","QCP","GWRE","BOLD","RRGB","AQMS","SVVC","ANW","RFP","ZYNE","GROW","EFII","OI","FGEN","FCEA","MEOH","PDLI","NPTN","CAKE","RDUS","FENG","SYNA","AMWD","PNR","CBRL","HGV","VSAR","GPRE","ACLS","INO","DEO","TRUE","BEAT","TEF","CONN","CQP","MIK","UNM","DSX","GBX","PEGA","HLTH","MEET","KIN","NVCR","BBG","LADR","CCK","ARA","INVA","LEG","OC","AJG","SMTC","SBAC","SND","VEON","PBI","ADC","FLT","AXS","PPDF","AMX","IDCC","MBT","TS","TEL","MUR","EXTR","EXR","FANH","KMG","BSTI","FMX","BGCP","HOLX","ARLZ","PLAY","MRTX","OSK","CVRR","LPI","ABMD","NSM","TREE","NBIX","PCRX","FRAN","SPB","ANDX","SIVB","AVDL","CROX","SPGI","BKS","AMBC","CTB","MTSI","CLB","CMRE","LEA","SCCO","SEMG","MXIM","HPT","GIII","MKSI","DK","ADS","JASO","DBD","AUPH","CLNE","REN","DHT","ZAGG","ICON","UNVR","QTS","TRN","FIZZ","SM","LH","EXK","EEX","PPC","TRVG","ORLY","SAGE","CALM","RYAM","BLDR","CHGG","STOR","LCI","ARRS","CPE","AMAG","MTOR","SNBR","RS","YRCW","GDDY","PWR","BBSI","FISV","MGA","PI","TOT","AVB","RDN","SPR","SNX","PTEN","MSI","HOV","SMG","SHW","PLNT","CVI","WLK","CTRL","SKT","AN","JAZZ","SRCI","LOGI","NDAQ","ASUR","BUFF","INFN","VCEL","MIDD","CHKP","COUP","WIX","SC","SUN","CIM","AER","SEE","PII","CBG","PSA","GKOS","UIS","AEIS","RSG","INFO","LPX","KANG","ATHN","SGEN","FMC","JILL","WRK","SONC","ETR","LSI","CYBR","SDRL","AVP","RY","ALEX","APD","BLMN","ALRM","CPRT","TELL","CARA","ETM","OFG","CTXS","CVE","WWE","DLPH","TECD","TAST","CNAT","BTI","CGNX","SCG","PSTI","GMLP","NUS","LGIH","UNFI","MTDR","VRA","CYOU","HLT","PEG","OMC","SD","NCR","ZOES","BEN","LFC","APO","GSM","OAK","UCTT","SSW","WPG","TAHO","KIM","RBS","RSPP","CG","SPPI","UN","TSEM","PGR","CLDX","ATHX","VRNS","SOHU","GPC","HCP","TDOC","IMMR","ATRA","BLDP","HZNP","WDR","RUN","ERI","RNG","SFLY","JMEI","RGNX","FRO","EBIO","RGR","BPMC","PFPT","GLYC","TRXC","FNV","PRGS","AGEN","SA","PBF","INFI","HABT","ABEO","AMKR","IPI","CRR","XEC","SSI","SOGO","HIBB","TD","EAT","VLY","MTG","BKE","ZG","RRR","CNDT","USCR","JKS","LGFA","EMES","MSG","ZBH","HMY","GGP","WELL","VIAV","CRAI","HRS","GOLD","CLS","MLM","EMN","NVTA","FTNT","VFC","GSS","MC","MKC","DXCM","SYY","RACE","ADTN","SAIL","ASMB","AES","DGX","PRTA","SAVE","FTK","BTU","GWPH","ATHM","OLN","PRGO","MYGN","HAIN","SPN","INSY","HUBS","TSRO","SFM","KRO","CHTR","AEP","CYH","BWA","RIGL","MPLX","TSCO","POST","SORL","INFY","LOXO","CMA","ZBRA","TMO","QURE","BZH","GGB","PCTY","IAG","VTR","VIRT","ZGNX","PETS","PFG","ALLY","FFIV","FRTA","BWP","VOYA","SJM","TIVO","UBS","KLAC","DATA","GDEN","STMP","LNC","AU","GOGO","REVG","RMTI","AHP","NGL","BDX","CMP","PAH","HALO","TRV","WTI","ACXM","MTZ","ASML","ALL","BEL","SRG","KNDI","APTV","BOFI","LYG","AVAV","AR","IR","DF","EQT","ARES","JACK","IPGP","BURL","IPG","NTRI","IDT","FCEL","NCMI","IRM","BHF","DVA","HP","QDEL","IDTI","LJPC","ARWR","OCN","NBL","GALT","BPT","TWNK","YUMC","MARK","RGLD","ADAP","DAN","BLL","WPX","WING","DQ","ROK","NVS","SYK","CHS","AVXS","EEP","FND","FANG","ASNA","VRAY","EVC","NYT","LGCY","ENPH","CFG","SSRM","QSR","CB","IEP","ZAYO","RDSB","TGI","CORT","TPR","EVRI","ACRX","DIN","TRTN","A","DOV","VHC","SEDG","ED","EW","CSOD","SNY","SSYS","INTU","ACIA","ARAY","CLX","GOOS","MFGP","IVZ","EGOV","CRZO","CSTM","TK","ABC","DISCA","HSY","GNW","NRZ","ORBK","EQIX","MUX","MMC","GNC","DPS","MTN","VBLT","FOXA","CBL","NG","BPL","LC","PPG","BRFS","HTHT","BGS","SNE","KNX","SQM","AAP","BAX","CCI","EVHC","SEAS","RDC","CLNS","WP","LBTYA","GSUM","PENN","NAP","YRD","WRLD","ILMN","ATI","ERF","MAS","CHRW","GLNG","ACHN","NS","AGO","CDE","PEGI","WGO","HCA","IRBT","AMRN","LYV","DDS","WCN","HRL","SHLX","RDNT","MNKD","FMSA","AQXP","NTLA","EFX","FMI","AGNC","THC","LPL","KTOS","HA","BYD","PAYC","TPX","ITUB","TXT","EVBG","CLMT","IONS","HDS","PBYI","ADMS","CLVS","WPZ","RYB","NCLH","ODP","WY","LOCO","IAC","SWK","COT","RDFN","DO","JUNO","PK","GRMN","BITA","ARRY","SHAK","ESPR","CTAS","AAXN","AET","PDCO","ITW","VEEV","HBAN","TKR","CVNA","CAH","GWW","ICHR","CMCM","KFY","TXMD","MZOR","DNKN","TSS","PLCE","KKR","PF","SAND","ARCC","ZIOP","ECA","GTT","GPOR","EIX","SNCR","NLNK","TTM","RL","CNC","NKTR","DHR","DISCK","TER","COTY","PGNX","IBKR","BCS","KMT","CP","LYB","KSU","NAK","COHR","SHPG","AXTA","JAG","SODA","HFC","PHM","MARA","BHP","CNQ","NVO","PRU","CJ","ECYT","PIR","UHS","BW","PAA","CHL","CWH","KERX","DVAX","ECL","DERM","OKE","DPZ","GTN","ESIO","VRTX","AMRS","PAAS","CC","LW","CLDR","HRTX","CA","EL","TTD","PSEC","FLEX","PLD","TAP","FAST","EXPR","GEL","TSN","WETF","SPG","BMRN","DNR","PTCT","AVEO","BLK","AMPE","XLNX","MDCO","USG","IMAX","PCAR","OHI","EROS","TSM","PLUG","VUZI","ADT","STLD","AXDX","PSX","FLR","TEAM","FCAU","EQR","TRGP","ZEN","MT","WM","UBNT","KL","IMMU","MIC","HUN","AFSI","ZTO","SU","PE","LE","WHR","ACAD","CRSP","HRB","HUM","WUBA","SGMS","ICPT","DXC","ACHC","FTI","HST","APPS","KODK","AKAM","HAS","LL","AKAO","XON","ETN","ALNY","PRTY","OMEX","STM","TGTX","CARS","CCJ","ZTS","AKRX","NYCB","TOL","WEN","PVH","ROST","AMC","PVG","AON","MMP","BBD","VMC","HCLP","CX","DECK","AEO","Z","XXII","KBR","VRSN","ZN","GRPN","CENX","KW","SKX","TEX","ARNA","CXO","O","GNCMA","ALK","DFS","PZZA","YUM","TLRD","CNHI","STT","ADI","GLIBA","CNX","MCHP","IBN","NSC","CBI","LEN","VIAB","NLY","FIVE","UPL","UNIT","ABB","EGO","CSIQ","XNET","NE","MLCO","KMX","SYMC","SLCA","AXL","PNC","EXEL","CAG","SAN","NTAP","FITB","HIG","ADP","JBLU","ALGN","CERN","EDIT","DLR","KGC","CTSH","FE","NBR","NEE","NUAN","TROW","RIO","NOK","ICE","CPB","NOV","WYN","FOLD","MAC","NVAX","FDC","MTCH","JWN","CRUS","OMER","RDSA","SHLD","CL","BSX","SINA","GBT","OPK","PX","PSTG","OKTA","CF","TROX","MCK","AZO","NTR","QRVO","MDR","ANTM","PCG","SRNE","ANDV","AEM","ATUS","SPWR","DUK","XPO","ARNC","MBI","COG","EXAS","KBH","DSW","GSK","MELI","RCL","ENB","SGMO","DRI","SFIX","NOG","GD","EDU","EXC","GT","IMGN","AZN","NGD","HL","ENDP","GES","GRUB","VOD","DVMT","ASH","SNA","MAR","JNPR","YPF","OAS","SO","AFL","ZNGA","DHI","HLF","CLR","YELP","D","URI","RCII","MPC","XL","BLUE","IGT","QEP","WDAY","UA","BBT","ETSY","TECK","HBI","PH","SVU","ACN","KEM","CRTO","HOME","NUE","BBBY","MET","TRIP","CMI","NAV","BHGE","VIPS","YNDX","PTLA","KORS","STI","LITE","ESV","WATT","EMR","BG","WSM","KEY","AOBC","W","WTW","RH","OCLR","TIF","THO","INCY","HOG","TAL","ZION","FSM","NOW","MAT","BK","HPE","PAYX","BHVN","DISH","ANTH","FTR","LLY","MON","KMB","IP","PPL","SGYP","BTG","REGN","APRN","NOC","FINL","WFT","PXD","MNK","TJX","CBOE","AAOI","ISRG","CS","ANET","ETFC","ADM","CREE","MOS","RAD","TTWO","JCI","COF","URBN","OXY","ONDK","NFX","XRX","CME","CAR","BOX","GLUU","WPM","MNST","WMB","CRC","AMT","FOSL","K","CBS","SIG","ANF","BIG","BIIB","DG","EOG","RF","ALXN","HES","RTN","JBL","SPLK","MDT","HON","CCL","LNG","NYLD","AG","NWL","TMUS","ON","ILG","MULE","BZUN","GPS","FL","SWKS","HTZ","ABT","HMNY","PM","AUY","GIS","AMBA","SRPT","RIOT","FNSR","ALB","USB","GLW","DVN","APA","WB","KHC","EPD","MSCC","AA","WBA","TWLO","EBIX","MYL","UNH","ESRX","FIT","LUV","HSBC","NRG","KSS","LMT","SWN","NXPI","HIMX","MDLZ","RRC","P","DLTR","RHT","SBGI","GG","NTES","BUD","PANW","CI","LFIN","ULTA","EXPE","STZ","AMTD","TWX","CSX","UAA","MDXG","NEM","UNP","PEP","FSLR","CY","SYF","MMM","OLED","COP","ETE","SCHW","VMW","EA","MRO","DDD","GPRO","YY","AXP","APC","SHOP","CTL","CTRP","BRKB","HIIQ","GERN","BBY","FEYE","VLO","QD","MOMO","BX","DB","KMI","NTNX","LULU","LVS","CZR","DE","TXN","BP","GME","VALE","RIG","MRVL","EBAY","CIEN","STX","CVS","WLL","ETP","LB","UAL","AAL","HPQ","S","DKS","AGN","MS","DWDP","MA","MGM","AIG","MO","PG","CMG","AABA","LOW","ROKU","OSTK","WDC","COST","LRCX","BKNG","PBR","SLB","ABBV","ATVI","TGT","ADSK","SIRI","PAY","FDX","UTX","V","CMCSA","HAL","SN","M","ABX","KO","AMGN","NKE","BB","PFE","AVGO","KR","JNJ","IBM","BMY","BIDU","JCP","ADBE","CHK","MCD","TEVA","GOOG","DAL","GILD","UPS","CVX","SBUX","MRK","AKS","CLF","GS","PYPL","HD","CRM","DIS","CELG","VZ","WYNN","JD","GOOGL","FCX","VRX","GM","AMAT","SNAP","CAT","WFC","CSCO","QCOM","ORCL","SQ","T","XOM","F","BA","JPM","X","WMT","C","TWTR","MSFT","TSLA","INTC","NVDA","AMZN","NFLX","AMD","BABA","GE","MU","BAC","AAPL","FB")
  
  if (symbols[1] == "spy100")
  {
    symbols <- SPY_100
  }
  
  if (symbols[1] == "dow")
  {
    symbols <- DOW_30
  }

  if (symbols[1] == "nasdaq")
  {
    symbols <- NASDAQ_30
  }
  
  if (symbols[1] == "nasdaq100")
  {
    symbols <- NASDAQ_100
  }

  if (symbols[1] == "equity")
  {
    equity <- c(SPY_100,NASDAQ_100,NASDAQ_30,DOW_30)
    symbols <- unique(equity)
  }
  
  if (symbols[1] == "all")
  {
    symbols <- OPTIONABLE
  }
  
  
  SUMMARY <- ""

  summary_index <- 0
  #print(LOOK_BACK)
  for(symIdx in 1:length(symbols))
  {
    underlyingURL <- getURL(symbols[symIdx],"UNDERLYING_END_POINT","")
    underlyingData <- getMarketData(underlyingURL);

    #print(underlyingData)
    
    optionChainURL <- getURL(symbols[symIdx],"OPTIONS_CHAINS_API_END_POINT",expiration) 
    optionsChainData <- getMarketData(optionChainURL)

    #print(optionChainURL)
    #print(optionsChainData)
    #print(length(optionsChainData$options$option))
    series_len <- 0
    tryCatch(series_len <- length(optionsChainData$options$option[,"symbol"]), error=function(e) print(paste(symbols[symIdx], "<NOT FOUND>")))

    #print(paste("len ", length(optionsChainData$options$option[,"symbol"])))
    for (i in 1:series_len)
    {
      if (series_len < 1)
      {
      #  print("**************************************************************")
      #  print(paste(symbols[symIdx],"<",expiration,">","[NO OPTIONS SERIES]"))
      #  print("**************************************************************")
        break
      }

      if(optionsChainData$options$option[i,"volume"] > seriesVolume & (optionsChainData$options$option[i,"volume"] > (optionsChainData$options$option[i,"open_interest"] * 2)))
      {
        #Prepare option quote url to get time of sales
        optionQuoteURL <- getURL(optionsChainData$options$option[i,"symbol"],"TIMESALES_API_END_POINT","")
        optionsQuoteData <- getMarketData(optionQuoteURL)
 
        sales_len <- 0
        tryCatch(sales_len <- length(optionsQuoteData$series$data[,"close"]), error=function(e) print(paste(optionsChainData$options$option[i,"symbol"], "<NOT FOUND>")))
       
        printed <- FALSE
        for(idx in 1:sales_len)
        {
          if (sales_len < 1)
          {
            #  print("**************************************************************")
            #  print(paste(symbols[symIdx],"<",expiration,">","[NO OPTIONS SERIES]"))
            #  print("**************************************************************")
            break
          }
          
          if (optionsQuoteData$series$data[idx,"volume"] > saleVolume)
          {
           
            summary_index <- summary_index + 1

            MONEYNESS <- ""

            SYMBOL_DETAIL <- paste(underlyingData$quotes$quote$symbol, "(", underlyingData$quotes$quote$description, ")")

            STRIKE_VS_SPOT <- round(optionsChainData$options$option[i,"strike"] - underlyingData$quotes$quote$close, digits = 2)

            STRIKE_PRICE_B_A <- paste(round(optionsQuoteData$series$data[idx,"close"],digits=4), "|", round(optionsChainData$options$option[idx,"bid"],digits=4), "x",round(optionsChainData$options$option[idx,"ask"],digits=4));

            if (toupper(optionsChainData$options$option[i,"option_type"]) == "CALL" )
            {
            	if (STRIKE_VS_SPOT > 0 )
            	{
            	MONEYNESS <- paste(str_pad(abs(STRIKE_VS_SPOT),6,"right"),"OM")
            	}
            	else
            	{
            	MONEYNESS <- paste(str_pad(abs(STRIKE_VS_SPOT),6,"right"),"IM")
            	}
            }
            else
            {
            	if ( STRIKE_VS_SPOT < 0 )
            	{
            	MONEYNESS <- paste(str_pad(abs(STRIKE_VS_SPOT),6,"right"),"OM")
            	}
            	else
            	{
            	MONEYNESS <- paste(str_pad(abs(STRIKE_VS_SPOT),6,"right"),"IM")
            	}
            }

            SUMMARY [[summary_index]] <- paste(str_pad(SYMBOL_DETAIL,25,"right"), "|", str_pad(underlyingData$quotes$quote$close,7,"right"),"|",str_pad(optionsChainData$options$option[i,"strike"],6,"right"),"|",str_pad(MONEYNESS,10,"right"),str_pad(toupper(optionsChainData$options$option[i,"option_type"]),4,"right"),"|",optionsChainData$options$option[i,"expiration_date"],"|",str_pad(optionsChainData$options$option[i,"volume"],9,"right"),"|",str_pad(optionsChainData$options$option[i,"open_interest"],8,"right"),"|",str_pad(optionsQuoteData$series$data[idx,"volume"],6,"right"),"(SV)", STRIKE_PRICE_B_A )

            if(printed == FALSE)
            {
            print("===================================================================================")
            print(paste(underlyingData$quotes$quote$symbol,underlyingData$quotes$quote$description, underlyingData$quotes$quote$close,optionsChainData$options$option[i,"root_symbol"],optionsChainData$options$option[i,"strike"],"[",optionsChainData$options$option[i,"option_type"],"]",optionsChainData$options$option[i,"expiration_date"],optionsChainData$options$option[i,"symbol"]," ",optionsChainData$options$option[i,"volume"]," | ",optionsChainData$options$option[i,"open_interest"]))
            print("--------------------------------------------------------------------------------")
            print(paste("Searching...",optionsChainData$options$option[i,"symbol"]," Time of Sales..."))
            print("--------------------------------------------------------------------------------")
           
            printed <- TRUE
            }
            #print(paste("[Found Large Order] at ",idx ))
            print(optionsQuoteData$series$data[idx,])

          }
        }
        
        printed <- FALSE
      }
    }
  }
   print("===================================================================================")
   print("[SUMMARY]")
   print("--------------------------------------------------------------------------------")
  for(x in 1:summary_index)
  {
    print(SUMMARY[x])
  }
   print("--------------------------------------------------------------------------------")
  sink()
}