# Testing 



#function to plot avg cov
plot_avg_cov <- function(df){
  ### load libraries
  library(ggplot2)
  
  ### reorder dataframes by chromosome 
  #make a vector  
  foo <- rep(0, nrow(df))
  #set order
  foo[with(df, chr == "chr1" )]=1
  foo[with(df, chr == "chr2" )]=2
  foo[with(df, chr == "chr3" )]=3
  foo[with(df, chr == "chr4" )]=4
  foo[with(df, chr == "chr5" )]=5
  foo[with(df, chr == "chr6" )]=6
  foo[with(df, chr == "chr7" )]=7
  foo[with(df, chr == "chr8" )]=8
  foo[with(df, chr == "chr9" )]=9
  foo[with(df, chr == "chr10" )]=10
  foo[with(df, chr == "chr11" )]=11
  foo[with(df, chr == "chr12" )]=12
  foo[with(df, chr == "chr13" )]=13
  foo[with(df, chr == "chr14" )]=14
  foo[with(df, chr == "chr15" )]=15
  foo[with(df, chr == "chr16")]=16
  foo[with(df, chr == "chr17" )]=17
  foo[with(df, chr == "chr18" )]=18
  foo[with(df, chr == "chr19" )]=19
  foo[with(df, chr == "chr20" )]=20
  foo[with(df, chr == "chr21" )]=21
  foo[with(df, chr == "chr22" )]=22
  foo[with(df, chr == "chrX" )]=23
  foo[with(df, chr == "chrY" )]=24
  foo[with(df, chr == "genome" )]=25
  #reset order in original df
  df$chr = with(df, reorder(chr, foo))
  ###graph
  ggplot(df, aes(chr, df[,4] )) +geom_bar(stat = "identity") + ylab("average coverage")
}
###e.g.
#plot_avg_cov(norm_avg_cov) + ggtitle("normal1 avg cov")

#function to plot avg cov for two dataframes (e.g. Tumor/Normal in same individual)
paired_avg_cov <- function(df1, df2){
  ### load libraries
  library(cowplot)
  library(ggplot2)
  
  ### reorder based on chromosome
  foo <- rep(0, nrow(df1))
  foo[with(df1, chr == "chr1" )]=1
  foo[with(df1, chr == "chr2" )]=2
  foo[with(df1, chr == "chr3" )]=3
  foo[with(df1, chr == "chr4" )]=4
  foo[with(df1, chr == "chr5" )]=5
  foo[with(df1, chr == "chr6" )]=6
  foo[with(df1, chr == "chr7" )]=7
  foo[with(df1, chr == "chr8" )]=8
  foo[with(df1, chr == "chr9" )]=9
  foo[with(df1, chr == "chr10" )]=10
  foo[with(df1, chr == "chr11" )]=11
  foo[with(df1, chr == "chr12" )]=12
  foo[with(df1, chr == "chr13" )]=13
  foo[with(df1, chr == "chr14" )]=14
  foo[with(df1, chr == "chr15" )]=15
  foo[with(df1, chr == "chr16")]=16
  foo[with(df1, chr == "chr17" )]=17
  foo[with(df1, chr == "chr18" )]=18
  foo[with(df1, chr == "chr19" )]=19
  foo[with(df1, chr == "chr20" )]=20
  foo[with(df1, chr == "chr21" )]=21
  foo[with(df1, chr == "chr22" )]=22
  foo[with(df1, chr == "chrX" )]=23
  foo[with(df1, chr == "chrY" )]=24
  foo[with(df1, chr == "genome" )]=25
  
  df1$chr = with(df1, reorder(chr, foo))
  ###make graph
  p1 <- ggplot(df1, aes(chr, df1[,4] )) +geom_bar(stat = "identity") + ylab("normal average coverage") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + background_grid(major = "xy", minor = "none")
  
  foo <- rep(0, nrow(df2))
  foo[with(df2, chr == "chr1" )]=1
  foo[with(df2, chr == "chr2" )]=2
  foo[with(df2, chr == "chr3" )]=3
  foo[with(df2, chr == "chr4" )]=4
  foo[with(df2, chr == "chr5" )]=5
  foo[with(df2, chr == "chr6" )]=6
  foo[with(df2, chr == "chr7" )]=7
  foo[with(df2, chr == "chr8" )]=8
  foo[with(df2, chr == "chr9" )]=9
  foo[with(df2, chr == "chr10" )]=10
  foo[with(df2, chr == "chr11" )]=11
  foo[with(df2, chr == "chr12" )]=12
  foo[with(df2, chr == "chr13" )]=13
  foo[with(df2, chr == "chr14" )]=14
  foo[with(df2, chr == "chr15" )]=15
  foo[with(df2, chr == "chr16")]=16
  foo[with(df2, chr == "chr17" )]=17
  foo[with(df2, chr == "chr18" )]=18
  foo[with(df2, chr == "chr19" )]=19
  foo[with(df2, chr == "chr20" )]=20
  foo[with(df2, chr == "chr21" )]=21
  foo[with(df2, chr == "chr22" )]=22
  foo[with(df2, chr == "chrX" )]=23
  foo[with(df2, chr == "chrY" )]=24
  foo[with(df2, chr == "genome" )]=25
  
  df2$chr = with(df2, reorder(chr, foo))
  ###graph
  p2 <- ggplot(df2, aes(chr, df2[,4] )) +geom_bar(stat = "identity") + ylab("tumor average coverage") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + background_grid(major = "xy", minor = "none")
  ###plot graphs together with cowplot 
  plot_grid(p1, p2, align='h', labels=c('N', 'T'))
}
### example:
# paired_avg_cov(norm_avg_cov,tumor_avg_cov) + ggtitle("Sample #1 avg coverage")