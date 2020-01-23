#' Incidence against actual cases plot
#' @author Adam Kucharski
#' @return
#' @export
#'
#' @examples
#'
plot_cases <- function(){

  actual_cases <- data
  start_date <- min(actual_cases$report_date)
  actual_cases_plot <- actual_cases %>%
    filter(report_date<start_date+define_6m)

  # Find period until moving average above 10
  # length_gap <- actual_cases[which((movavg(actual_cases$total_cases_change,7)>10)==T)[1],]$report_date - start_date
  # 239 days

  # Total cases at this point
  actual_cases %>%
    filter(report_date<start_date+define_6m) %>%
    select(total_cases) %>%
    max()

  plot(actual_cases_plot$report_date, actual_cases_plot$total_cases_change,type="l",
       ylim=c(0,50), ylab="cases", xlab="2018/19")

  file_chains <- list.files(chains)

  date_seq <- seq(start_date,start_date+define_6m,1)

  for(i in 1:n.sim){
    chain_data <- read_csv(paste0(chains,"/", file_chains[i])); chain_n <- nrow(chain_data)
    chain_data <- chain_data %>% filter(!is.na(onset.t),onset.t>0) # Extract cases
    onset_plot <- start_date + round(chain_data$onset.t)

    tally_onset <- sapply(date_seq,function(x){sum(round(onset_plot)==x)})

    lines(x=date_seq,y=tally_onset,col=rgb(1,0,0,output_df_Base$prob_select[i]/max(output_df_Base$prob_select) ))
  }


  #plot(actual_cases$report_date,actual_cases$total_cases_change,type="l",ylim=c(0,20),ylab="cases",xlab="2018/19")





}
