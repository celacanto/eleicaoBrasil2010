# Packages
# ===================================================================

library(XML)



# Functions
# ===================================================================

getLinks <- function(turno, tipos){

  #### The URL of the TSE where the links for the files are  
  if(turno == 2){
    addToUrl = "_2t"
  } else{
    addToUrl = ""
  }
  tse_url <- paste0("http://www.tse.jus.br/hotSites/estatistica2010/arquivos_para_download", 
                    addToUrl, 
                    "/totalizacao.html")    

  #### Read the page
  tse_html <- htmlParse(tse_url)
  
  ### Get the links of the files
  estado_links <- unlist(xpathApply(tse_html, "//p[*]", xmlValue, trim=TRUE))
  # Just the links (remove the states names)
  link <- gsub("^.*\r\n\\s+", "", estado_links)
  # The links can refer to diferent informations
  tipo_link <- gsub("^.+/|_[2A-Z].+$", "", link)
  links_por_tipo <- split(link, tipo_link)
  
  # Return just the ones asked
  links_por_tipo[tipos]
}


getTxtFile <- function(links, dir){

  ### Create a dir where the files are stored
  dir.create(dir)
  
  ### The names of the zip file in the dir
  filesOut <- sapply(links, function(x) paste0(dir, "/", gsub(".+/", "", x)), USE.NAMES = FALSE)
  
  ### Download the files
  for(i in seq_along(links)){
    # download files
    download.file(links[i], filesOut[i])
  }
  
  # Unzip the files to the txt files and remove the zip file
  for(zip in filesOut){
    unzip(zip, exdir = dir)
    file.remove(zip)
  }
  return(gsub(".zip$", ".txt", filesOut))
}


getMZdetails <- function(file, turno){


  table <- read.csv(file, stringsAsFactors = FALSE, header = FALSE, sep = ";", fileEncoding = "latin1")
  
  ### If is second rowd the las line has the column header
  ### If is the fisrt rowd there is no header
  ### So we remove the last line (if second rowd) and put the name manualy
  if(turno == 2){
    table <- table[-nrow(table), ]  
  }
  colnames(table) <- c("DATA_GERACAO","HORA_GERACAO","COD_ELEICAO","DESC_ELEICAO",
                       "DATA_ELEICAO","NUM_ANO","NUM_TURNO","SIGLA_UF",
                       "COD_MUNICIPIO","NOME_MUNICIPIO","NUM_ZONA","COD_CARGO",
                      "DESCRICAO_CARGO","QTD_APTOS","QTD_APTOS_TOT",
                       "QTD_SECOES","QTD_SECOES_AGREGADAS","QTD_SECOES_TOT","QTD_COMPAREC",
                       "QTD_ABSTENCOES","QTD_VOTOS_NOMINAIS",
                        "QTD_VOTOS_BRANCOS","QTD_VOTOS_NULOS","QTD_VOTOS_LEGENDA","QTD_VOTOS_VALIDOS",
                        "QTD_VOTOS_ANULADOS","QTD_IMPUGNACOES",
                        "QTD_RECURSOS","DATA_ULT_TOTALIZACAO","HORA_ULTIMA_TOTALIZACAO")
                        
      
    ### Just return the column that we have interest in
    table <- table[, c("SIGLA_UF", "COD_MUNICIPIO",  "NOME_MUNICIPIO","NUM_ZONA", "DESCRICAO_CARGO", 
                      "QTD_APTOS_TOT", "QTD_COMPAREC")]    
}


getVMZcandidato <- function(file, turno){

  table <- read.csv(file, stringsAsFactors = FALSE, header = FALSE, sep = ";", fileEncoding = "latin1")
  
  
  ### If is second rowd the las line has the column header
  ### If is the fisrt rowd there is no header
  ### So we remove the last line (if second rowd) and put the name manualy
  if(turno == 2){
    table <- table[-nrow(table), ]  
  }
  colnames(table) <- c("DATA_GERACAO","HORA_GERACAO","COD_ELEICAO","DESC_ELEICAO","DATA_ELEICAO",
                        "NUM_ANO","NUM_TURNO","SIGLA_UF",
                       "COD_MUNICIPIO","NOME_MUNICIPIO","NUM_ZONA","COD_CARGO","DESCRICAO_CARGO", 
                       "NUM_PARTIDO", "NOME_URNA","NOME_CANDIDATO",
                       "COD_SITUACAO_CAND","DESC_SITUACAO_CAND","COD_SITUACAO_CAND_SUP","DESC_SITUACAO_CAND_SUP",
                       "COD_SITUACAO_TOT", "DESC_SITUACAO_TOT","COD_FAIXA_ETARIA","DESC_FAIXA_ETARIA",
                       "COD_SEXO","DESC_SEXO","NUM_PARTIDO","SIGLA_PARTIDO", "NOME_LEGENDA","COMPOSICAO_LEGENDA",
                       "COD_TIPO_VOTAVEL","DESC_TIPO_VOTAVEL","DATA_ULT_TOTALIZACAO","HORA_ULT_TOTALIZACAO", "NUM_VOTOS")
                       
  ### Just return the column that we have interest in
  table <- table[, c("SIGLA_UF", "COD_MUNICIPIO",  "NOME_MUNICIPIO", "NUM_ZONA", "DESCRICAO_CARGO", 
                    "DESC_SITUACAO_TOT", "NUM_VOTOS", "NOME_CANDIDATO")]  
}



# Do
# ===================================================================

# The type tables that we will need
tipos <- c("detalhe_munzona", "vmz_candidato")

# Get the links of the tables
links <- getLinks(turno = 1, tipos = tipos)

# Download the tables
for(tp in tipos){
  txtFiles[[tp]] <- getTxtFile(links[[tp]], tp)
}


# Join the tables (they were by states)
detalhes_mz <- do.call(rbind, lapply(txtFiles[["detalhe_munzona"]], getMZdetails, turno = 1))
candidato_vmz <- do.call(rbind, lapply(txtFiles[["vmz_candidato"]], getVMZcandidato, turno = 1))

# Merge the two types of tables
eleicaoPais <- merge(detalhes_mz, candidato_vmz)

# Make DEPUTADO DISTRITAL AND DEPUTADO ESTADUAL THE SAME THING
eleicaoPais$DESCRICAO_CARGO <- gsub("DEPUTADO DISTRITAL", "DEPUTADO DISTRITAL OU ESTADUAL", eleicaoPais$DESCRICAO_CARGO)
eleicaoPais$DESCRICAO_CARGO <- gsub("DEPUTADO ESTADUAL", "DEPUTADO DISTRITAL OU ESTADUAL", eleicaoPais$DESCRICAO_CARGO)

# Split the table by state
eleicaoPerUF <- split(eleicaoPais, eleicao$SIGLA_UF)


# The roles that we are interested in
cargos <- c("GOVERNADOR", "SENADOR")


for(cargo in cargos){
  
    png(paste0(cargo, ".png"), height = 4050, width = 900, res = 120)
    
    par(mfrow=c(9,2))      
    
  
    for(uf_name in names(eleicaoPerUF)){
      
        
      uf <- eleicaoPerUF[[uf_name]]  
      
      uf_cargo <- uf[uf$DESCRICAO_CARGO == cargo,]

      politicos <- unique(uf_cargo$NOME_CANDIDATO)
      vencedor <- names(sort(sapply(politicos, function(x) sum(subset(uf_cargo, subset = NOME_CANDIDATO == x, select = NUM_VOTOS))), decreasing = TRUE))[1]
      
      table <- uf_cargo[uf_cargo$NOME_CANDIDATO == vencedor,]
      if(nrow(uf_cargo) < 500) next
        
          
    
      table$x <- as.numeric(table$QTD_COMPAREC)/as.numeric(table$QTD_APTOS_TOT) * 100
      table$y <- as.numeric(table$NUM_VOTOS)/as.numeric(table$QTD_COMPAREC) * 100
      
      dens <- densCols(table$x, table$y, colramp=colorRampPalette(c("black", "white")))
      table$dens <- col2rgb(dens)[1,] + 1L
      
      # Map densities to colors
      cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", 
                                "#FCFF00", "#FF9400", "#FF3100"))(256)
      table$col <- cols[table$dens]
      axis_lim = c(0,100)
      plot(y~x, data=table[order(table$dens),], pch=20, col=col, cex=2, 
           xlim = axis_lim, ylim = axis_lim, main = uf_name, 
           xlab = "Porcentagem de comparecimento", ylab = "Porcentagem de votos ao vitorioso")
  }
  dev.off()  
  
}

presidente <- eleicaoPais[eleicaoPais$DESCRICAO_CARGO == "PRESIDENTE",]
table <- eleicaoPais[eleicaoPais$NOME_CANDIDATO == "DILMA VANA ROUSSEFF",]

table$x <- as.numeric(table$QTD_COMPAREC)/as.numeric(table$QTD_APTOS_TOT) * 100
table$y <- as.numeric(table$NUM_VOTOS)/as.numeric(table$QTD_COMPAREC) * 100

dens <- densCols(table$x, table$y, colramp=colorRampPalette(c("black", "white")))
table$dens <- col2rgb(dens)[1,] + 1L

# Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", 
                            "#FCFF00", "#FF9400", "#FF3100"))(256)
table$col <- cols[table$dens]
axis_lim = c(0,100)
plot(y~x, data=table[order(table$dens),], pch=20, col=col, cex=2, 
    xlim = axis_lim, ylim = axis_lim, main = "Presidente -- Brasil", 
     xlab = "Porcentagem de comparecimento", ylab = "Porcentagem de votos ao vitorioso")
