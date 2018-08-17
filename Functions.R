### xdoan
### useful functions and commands


##-----------------------------------------------##
## 1 
## function to barplot by chr for one dataframe 
##-----------------------------------------------##
plot_bar_chr <- function(df){
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
plot_bar_chr(norm_avg_cov) + ggtitle("normal1 avg cov")

##-----------------------------------------------##
## 2 
## #function to barplot by chromosome for two dataframes 
## (e.g. Tumor/Normal in same individual)
##-----------------------------------------------##
paired_bar_chr <- function(df1, df2){
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
paired_bar_chr(norm_avg_cov,tumor_avg_cov) + ggtitle("Sample #1 avg coverage")

##-----------------------------------------------##
## 3
## Split a dataframe by chromosomes 
## Get normalized counts by chromosome
## Merge dataframe, delete split vector of df 
##-----------------------------------------------##
normalized_counts <- function(df_all_chr, df_avg_coverage){
  by_chr <- split (df_all_chr, df_all_chr$chr)

  for(i in seq_along(by_chr)){
    counts <- (getElement(by_chr[[i]], "counts"))
    norm_c <- counts/df_avg_coverage[i,4]
    by_chr[[i]] <- as.data.frame(by_chr[[i]])
    by_chr[[i]]$norm_counts <- norm_c
    }
  df_all_chr <- unsplit(by_chr, f=df$chr)
  rm(by_chr)
}

##-----------------------------------------------##
## 4
## Facets a jitter plot by chromosome 
## draws colors over the facet grid label that matches the chromosome color
## Custon theme to decrease the lines on the plot but keep some, since there are lots of chromosomes
##-----------------------------------------------##
g <- ggplot(all_innerjoin, aes(x = chr , y = difference, color=chr)) +
  geom_jitter(aes(alpha=.5)) + theme(axis.text=element_text(size=12), axis.title = element_text(size=15), plot.title = element_text(size=20), axis.text.x = element_text(angle = 90, hjust = 1), 
  panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 1, linetype = "solid"),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                colour = "gray95"),
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "gray95") )  + 
  scale_color_manual(values=c("#01953b", "#f67bff", "#39e270", "#db31b0", "#557100", "#9d67f2", "#b76700", "#1749bb", "#b70d02", "#28caff", "#fc257a", "#02aea1", "#ff5a62", "#0090c7", "#ff9a6c", "#648dff", "#e1c286", "#0162b8", "#80d6bd", "#ff63ad", "#545182", "#ffaca8", "#922372", "#ddb4ff") ) + scale_alpha(guide = 'none') + ggtitle("Pooled Difference in Head and Neck Cancer Patients") + ylab("Difference (T-N)") + xlab("Array") + facet_grid(.~chr, scale = "free",space = "free_x") + guides(colour=FALSE) 

g <- ggplot_gtable(ggplot_build(g))

strip_t <- which(grepl('strip-t', g$layout$name))
#make sure they match the scale_color_manual values!
fills <- c("#01953b", "#f67bff", "#39e270", "#db31b0", "#557100", "#9d67f2", "#b76700", "#1749bb", "#b70d02", "#28caff", "#fc257a", "#02aea1", "#ff5a62", "#0090c7", "#ff9a6c", "#648dff", "#e1c286", "#0162b8", "#80d6bd", "#ff63ad", "#545182", "#ffaca8", "#922372", "#ddb4ff")
k <- 1

for (i in strip_t) {
j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
k <- k+1
}
grid.newpage()
grid.draw(g)
