###*********************************************###
### HOTspots: Tracking Antimicrobial Resistance ###
###*********************************************###
# Author: Alys Young
# Collaborators: Saras Windecker and Nick Golding
#
# Project aim: Create a shiny app to map antimicrobial resistance 
# Script aim: Objects that will likely be editted in the future by other users and collaborators


### Dates -----------------------------------------------------------------------------------------------------------------------------------------------
# Date the data was last updated
# Extracting the date from the file did not work once deployed to shinyapps.io
date_updated <- "15-Mar-2021"
year_updated <- "2021"

# Todays date
date_retrieved <-  format(Sys.time(), '%d %B, %Y')

# Date the methods were updated
date_method_update <- "13-Mar-2021"


### Details -----------------------------------------------------------------------------------------------------------------------------------------------
# Contact email
contact_email <- "Teresa.Wozniak@csiro.au" 

# Citation
# how_to_cite <- paste0('How to cite: Menzies School of Health Research. ', year_updated, '. HOTspots. Darwin, Northern Territory. Retrieved from https://anti-microbe-res.shinyapps.io/hotspots/ on ', date_retrieved, '.')

# Share to social media URL
website_url_clean <- "https://anti-microbe-res.shinyapps.io/hotspots/"

# Share to social media message
website_message_clean <- "Track antimicrobial resistance with this new tool from CSIRO"


### Publications -----------------------------------------------------------------------------------------------------------------------------------------------
publication_list <- fluidRow(
  
  # To add more publications, use the format below
  #p(tags$a(href="link doi", "Name"), "Title. Journal, edition(volume), page),
  
  p(tags$a(href = "https://www.nature.com/articles/s41598-020-69312-4", "TM Wozniak, W Cuningham, S Buchanan, et al., (2020)."), "Geospatial epidemiology of Staphylococcus aureus in a tropical setting: an enabling digital surveillance platform. Scientific Reports 10, 13169."),
  p(tags$a(href = "https://academic.oup.com/cid/advance-article/doi/10.1093/cid/ciaa1228/5895480", "X Lee, A Stewardson, L Worth, N Graves, TM Wozniak (2020)."), "Attributable length of stay, mortality risk and costs of bacterial healthcare-associated infections in Australia: a retrospective case-cohort study. Clinical Infectious Diseases 72(10); 506–514"),
  p(tags$a(href = "https://peerj.com/articles/9409/", "W Cuningham, et al., (2020)."), "Antimicrobial stewardship in remote primary healthcare across northern Australia. PeerJ 8:e9409"),
  p(tags$a(href = "https://onlinelibrary.wiley.com/doi/full/10.1111/1753-6405.12876", "W Cuningham, et al., (2019)."), "High burden of infectious disease and antibiotic use in early life in Australian Aboriginal communities. Australian and New Zealand journal of public health, 43(2); 149-155."),
  p(tags$a(href = "https://www.cambridge.org/core/journals/infection-control-and-hospital-epidemiology/article/abs/health-and-economic-burden-of-antimicrobialresistant-infections-in-australian-hospitals-a-populationbased-model/A51CA4B0F6181C891F0B406823460C30", "TM Wozniak, E Bailey, N Graves (2019)."), "Health and economic burden of antimicrobial-resistant infections in Australian hospitals: a population-based model. Infection Control & Hospital Epi 40(3); 320-327."),
  p(tags$a(href = "https://aricjournal.biomedcentral.com/articles/10.1186/s13756-019-0472-z", "TM Wozniak, L Barnsbee, X Lee, R Pacella (2019)."), "Using the best available data to estimate the cost of antimicrobial resistance: a systematic review. Antimicrobial Resistance and Infection Control 8:26."),
  p(tags$a(href = "https://aricjournal.biomedcentral.com/articles/10.1186/s13756-018-0379-0", "TM Wozniak (2018)."), "Estimating the burden of antimicrobial resistance. Antimicrobial Resistance and Infection Control 7:91."),
  p(tags$a(href = "https://www.sciencedirect.com/science/article/pii/S2468045117302286", "TM Wozniak (2018)."), "Clinical management of drug-resistant bacteria in Australian hospitals: an online survey of doctors' opinions. Infection, Disease & Health 23(1); 41-48."),
  p(tags$a(href = "https://pubmed.ncbi.nlm.nih.gov/30479305/", "TM Wozniak, N Graves, A Barnett (2018)."), "How much do superbugs cost? An evidence-based open- access tool. Infection, Disease & Health 23(1); 54-56."),
  p(tags$a(href = "https://bmchealthservres.biomedcentral.com/articles/10.1186/s12913-017-2079-5", " Page, et al., (2017)."), ". What is a hospital bed day worth? A contingent valuation study of hospital Chief Executive Officers. BMC Health Services Research 17:137."),
  p(tags$a(href = "https://idhjournal.com/article/S2468-0451(17)30067-6/fulltext", "TM Wozniak, D Paterson, K Halton (2017)."), "Review of the epidemiological data regarding antimicrobial resistance in gram(-) bacteria in Australia. Infection, Disease & Health 22(4); 210-218."),
  p(tags$a(href = "https://www.idhjournal.com.au/article/S2468-0451(16)30192-4/fulltext", "J Cameron, L Hall, TM Wozniak, K Halton (2016)."), "The burden of community onset MRSA in Australia Infection, Disease & Healt. 21(3); 140.")
  
)

# # Consider making this into a function but idk how is best for the user to enter the details.
# make_publication_list <- function(author, year_published, details, url_doi ){
#   p <- p(tags$a(href = url_doi, paste0(author, ", (", year_published, ")."), details))
#   return(p)
# }
# p <- make_publication_list(author = "Alys Young", year_published = "2021", details = "other things here", url_doi = "www.google.com")
