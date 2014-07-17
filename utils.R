getDataWResults <- function(dataset){
  dataset[which(dataset$rezultat_final == "Reusit" | dataset$rezultat_final == "Respins"),];
}


getRowsWhereFieldContainsPattern <- function(dataset, field, pattern){
  
  dataset[which(grepl(pattern, x = dataset[[field]], ignore.case = TRUE)),]
  
}

getAveragesForSet <- function(dataset){
  
  result = vector(mode="numeric", length = 0);
  
  for(i in 1:nrow(dataset)) {
    
    numSubjects = 0;
    total = 0;
    
    total = total + as.numeric(as.character(dataset[i, "d_romana_scris_nota_finala"]));
    numSubjects = numSubjects + 1;
    
    
    lbMat = as.character(dataset[i, "d_limba_materna_scris_nota_finala"]);
    if( nchar(lbMat) > 0   ){
      total = total + as.numeric(lbMat);
      numSubjects = numSubjects + 1;
    }
    
    total = total + as.numeric(as.character(dataset[i, "d_profil_scris_nota_finala"]));
    numSubjects = numSubjects + 1;
    
    total = total + as.numeric(as.character(dataset[i, "d_alegere_scris_nota_finala"]));
    numSubjects = numSubjects + 1;
    
    
    avg = total / numSubjects;
    if(!is.nan(avg)){
      result = c(result, avg);
    }
  }
  
  return(result);
}

plotFieldAvgGrouped = function(dataset, field, group){
  
  avgs = vector(mode = "numeric", length = 0);
  groups = vector(mode = "character", length = 0);
  
  for(level in levels(dataset[[group]])){
    
    avg = ave(dataset[which(dataset[[group]] == level),][[field]])[1];
    
    avgs = c(avgs, avg);
    groups = c(groups, level);

  }  

  df = data.frame(avgs, groups);
  df = df[order(-df[,1]),][1:10,];
  
  df[,1] = round(df[,1], 2);
  
  par(mar = c(3.1, 3.1, 12.1, 2.1), xpd = TRUE)  
  barplot(df[,1], col = rainbow(  length(df[,2])  ), names.arg = df[,1]);
  
  legend("bottomleft", inset = c(0, 1.05), fill = rainbow(  length(df[,2])  ), legend = df[,2]);
}

plotFieldEqualsValueByGroup = function(dataset, field, value, group, percent = TRUE){
  counts = vector(mode = "numeric", length = 0);
  groups = vector(mode = "character", length = 0);
  
  for(level in levels(dataset[[group]])){
    
    count = length(dataset[which(dataset[[group]] == level & dataset[[field]] == value),1]);
    total = length(dataset[which(dataset[[group]] == level,),1]);
    
    
    if(percent){
      counts = c(counts, (count * 100) / total);
    }
    else{
      counts = c(counts, count);
    }
    
    groups = c(groups, level);    
  }
  
  df = data.frame(counts, groups);
  df = df[order(-df[,1]),];
  df = df[order(-df[,1]),][1:10,];
  
  df[,1] = round(df[,1], 2);
  
  par(mar = c(3.1, 3.1, 12.1, 2.1), xpd = TRUE)  
  barplot(df[,1], col = rainbow(  length(df[,2])  ), names.arg = df[,1]);
  
  legend("bottomleft", inset = c(0, 1.05), fill = rainbow(  length(df[,2])  ), legend = df[,2]);
}