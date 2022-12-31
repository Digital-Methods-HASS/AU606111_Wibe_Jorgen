
# Tobias Widmann
# Course: Computational Text Analysis
# Date: 22/02/2022

# Week 4: Web Scraping I
# Scraping Tables & Static Websites


#### Start ##################################################
# Clearing environment
rm(list = ls())

# Free unused R memory
gc()

# Setting working directory
getwd()  # returns current working directory
# SET THIS WORKING DIRECTORY TO YOUR WD PATH
setwd("/Volumes/GoogleDrive/My Drive/Work/Teaching/Computational Text Analysis/content/week4")  

# load packages
library(dplyr)
library(magrittr)
library(stringr)


#### Rvest ######################################

# First, we need to load the package we'll use for most of our scraping. 
# It is called *rvest*. 

library(rvest)

# Next, read in (sometimes called: parse) the webpage of which you copied the URL. 
# To tell R to read the webpage, we can use the function `read_html()`


url <- "https://en.wikipedia.org/wiki/World_Health_Organization_ranking_of_health_systems_in_2000"
url


searchresults <- read_html(url)
searchresults

# `read_html()` reads in the source code of the website and returns an object
# This is a file that contains both the text of the web page as well as a long list of 
# instructions about how the text, images, and other components of the webpage should be 
# rendered by the browser

# How does the code of a website look like?
browseURL("https://en.wikipedia.org/wiki/World_Health_Organization_ranking_of_health_systems_in_2000")

# You can learn more about HTML here if you like:
browseURL("https://www.w3schools.com/html/default.asp")

##### 1. Tables ###########################
# We start with something satisfying: Out of the box, `rvest` converts tables 
# into a list of data frames when calling `html_table()` on a page. 
# Use this on the webpage you parsed.

html_table(searchresults)

# If you assign the result to an object, the object will be a list.
# You can extract specific tables from this list by subsetting the list 
# (that is, putting the number of the table you want in two squared brackets). 
# Or, if you want to proceed in a piping-chain, you can use the command `extract2()` 
# from the `magrittr` package, adding the number of the table in brackets 
# (the command name is no typo - `extract` without the 2 works for vectors, 
# `extract2()` works for lists).

tables <- html_table(searchresults)
tables[[1]]


read_html(url) %>%
  html_table() %>% 
  extract2(1)

?extract2

# The table we scraped so far was an easy case because the table was very regular. 
# When tables are irregular (e.g. have different numbers of cells in each row), 
# the single most important specification for the command is the `fill` parameter. 
# If you specify fill as true inside the `html_table()` command, 
# rvest will automatically fill rows with fewer than the maximum number of columns with NAs. 

# Try it out on another Wikipedia page.
browseURL("https://en.wikipedia.org/wiki/List_of_members_of_the_Folketing,_2019%E2%80%932023")
url <- "https://en.wikipedia.org/wiki/List_of_members_of_the_Folketing,_2019%E2%80%932023"

tables <- read_html(url) %>% html_table(fill=T)
df <- extract2(tables,3)

df

###### 1.21 EXERCISE #############################################


# Check this list of Eiffel Tower replicas and derivatives: https://en.wikipedia.org/wiki/Eiffel_Tower_replicas_and_derivatives
# Start with scraping the table. 

page <- read_html("https://en.wikipedia.org/wiki/Eiffel_Tower_replicas_and_derivatives")

table <- html_table(page, fill = T)  %>% 
  extract2(1) 
table

# Then count which country has the most Eiffel tower replicas. You can use the tally() command

table %>% 
  group_by(Country) %>% 
  tally() %>% 
  slice_max(n)

table %>% 
  group_by(Country) %>% 
  tally() %>% 
  arrange(desc(n))

# ggplot
library(ggplot2)
ggplot(table,aes(x=Country))+
  geom_histogram(stat="count")+
  theme(axis.text.x = element_text(angle = 90))

# can you also identify the tallest replica?
# There is a height column but we would first need to extract the number
# and then turn it into numeric and order it from tallest to smallest

table %>% 
  mutate(Height=as.numeric(str_extract(Height, "\\d*"))) %>% 
  arrange(desc(Height))



##### 2. Extracting HTML tags ############################

# Now we got tables - but how do we get the rest of the webpage?

# The function `read_html()` parses the html code, similar to what our 
# browser does. Still, it gives us the entire document including the HTML commands.

# Since we do not want the formatting of the webpage, we can use the function html_text() to extract the Webpage text.

# Let's try it with a speech by the Danish prime minister.
browseURL("https://english.stm.dk/the-prime-minister/speeches/prime-minister-mette-frederiksen-s-new-years-speech-on-the-1st-of-january-2022/")

speech <- read_html("https://english.stm.dk/the-prime-minister/speeches/prime-minister-mette-frederiksen-s-new-years-speech-on-the-1st-of-january-2022/")

html_text(speech)

# Did you find the speech you saw in your browser? 
# Admittedly, this still looks very messy. 
# If only, there would be a way to tell R to just get the text of the speech! 
# Luckily there is.

###### 2.1 CSS Selectors ###############################################

# The html_nodes() command allows us to select specific 'nodes', that is, 
# elements of the HTML Code. 
# One example would be the HTML tags and their respective content. 

# HTML tags store content in an HTML code
# You can learn more about specific HTML tags here: https://www.w3schools.com/html/default.asp
# Using selectors, you can select specific HTML tags
# If you write the name of a tag (without the brackets), 
# the CSS selector will select all elements with that tag. 
# For example, "h1" will select everything inside a `<h1>` tag

# h... tags are often used for headlines
html_nodes(speech,"h1")
# p... tags are often used for paragraphs
html_nodes(speech,"p")

# a very useful selector is the asterisk - it just selects *all* tags in the page (so we also call it a universal selector)
html_nodes(speech,"*")

# as you can see, the entire node is collected: tags and content
# You can also extract the text by using the html_text() function

html_nodes(speech,"h1") %>% html_text()
html_nodes(speech,"p") %>% html_text()

###### 2.2 Exercise ################################

# We will learn how to select parts of specific webpages soon. 
# However, since webpages are built with a similar architecture, often, 
# we get quite far by selecting generic elements.

# Visit the webpage of the guardian (https://www.theguardian.com/) and 
# extract the headlines of different levels until you do not find anything anymore.
# Can you identify how the different levels are used in the webpage by looking at the 
# original page? 
# Remember that since this is a new webpage, you will have to  
# combine the commands we have learned so far, namely `read_html()`, `html_nodes()` and `html_text()`.

guardian <- read_html("https://www.theguardian.com/")
guardian %>% html_nodes("h1") %>% html_text()
guardian %>% html_nodes("h2") %>% html_text()
guardian %>% html_nodes("h3") %>% html_text()
guardian %>% html_nodes("h4") %>% html_text()
guardian %>% html_nodes("h5") %>% html_text()


##### 3. Extracting Specific Content ############################

###### 3.1 Using "Inspect" in your webbrowser ###################

html_nodes(speech,"time") %>% html_text()

html_nodes(speech,"span") %>% html_text()




###### 3.2 SelctorGadget ##############################

# While understanding HTML helps, we often do not need to engage with the 
# code because there are lots of tools to help us. 
# That is: We can get some help at doing many of the things we just learned

# For example, SelectorGadget is a JavaScript bookmarklet that 
# allows you to interactively figure out what css selector you need to extract
# parts of the page. If you have not heard of selectorgadget, 
# check its [webpage](https://selectorgadget.com/).

# 1. Click on the element you want to select. SelectorGadget will make a first guess at what css selector you want 
# and mark all similar elements. 
# It's likely to be a bad guess since it only has one example to learn from, but it's a start. 
# Elements that match the selector will be highlighted in yellow.
# 2. Click on elements that shouldn't be selected. They will turn red.  
# Click on elements that *should* be selected but are not so far. They will turn green.
# 3. Iterate until only the elements you want are selected.  SelectorGadget is not perfect and sometimes will not be able to find a useful css selector. 
# Sometimes starting from a different element helps, sometimes having a look at the HTML code and thinking of the grammar we learned above will help you.



# Let's try to use SelectorGadget
browseURL("https://www.presidency.ucsb.edu/documents/address-the-democratic-national-convention-4")

# Read in the webiste/parse the HTML code
speech <- read_html("https://www.presidency.ucsb.edu/documents/address-the-democratic-national-convention-4")

# Only select the text (the part you want to scrape)
selected_nodes <- html_nodes(speech,".field-docs-content p")
selected_nodes

# This already looks more structured - but we should get rid of the HTML tags.
# Apply the `html_text()` command we used before to the nodes which 
# we selected in the last step. This way, we get just the text from the nodes we selected.

text <- html_text(selected_nodes)
text

# You can collapse the multiple strings into one (results in one text string)
text <- str_c(text, collapse = " ")
text

# Now let's extract something else
# e.g. the date of the speech

date_node <- html_nodes(speech,".date-display-single")
date_node
date <- html_text(date_node)
date

# How about the headline?
h_node <- html_nodes(speech,"h1")
h_node
headline <- html_text(h_node)
headline

# How about the speeker?
speaker_node <- html_nodes(speech,".diet-title a")
speaker_node
speaker <- html_text(speaker_node)
speaker

df1 <- data.frame(speaker = speaker, headline = headline, date = date, speech = text)

df1

#### 4. Extracting Links ##########################################

# So, you have learned how to use some of the basic functions of the `rvest` package: 
# `read_html()`, `html_nodes()`, `html_table()` and `html_text()`.
# But most of the time, we are not just interested in a single page 
# but multiple pages from the same domain, e.g. all speeches by a prime minister or president

# So we need another step: We have to learn to follow links without 
# actually opening the browser and clicking on the link and copying the new path.
# So now we will learn how:
# - to extract links from webpages
# - to automate following these links

###### 4.1 Extracting Links #######################################

#  the `rvest` command to get these links is called `html_attr()`

# We can use it to extract different types of attributes, so you will have to tell rvest the attribute that we are interested in is a link. 
# Links often look like this in HTML

# `This is text <a href="https://www.presidency.ucsb.edu/">with a link</a>.`

# href stands for hyperreference and signifies the webpage the link leads to. 
# You can specify `name="href"` inside the `html_attr()` command to extract the link.

# However, this will only work on individual HTML tags not on entire pages (since the link is an attribute of the specific tag, not the page), so we will use `html_nodes()` again.

# Let's try this on all Obama speeches that mention Europe
browseURL("https://www.presidency.ucsb.edu/advanced-search?field-keywords=Europe&field-keywords2=&field-keywords3=&from%5Bdate%5D=&to%5Bdate%5D=&person2=200300&items_per_page=25")

# 1) you can extract potential links from all HTML tags by using the **universal selector** (`html_nodes("*")`)
# 2)look up the selector of the **speeches** and extract the links from those tags

searchresults <- read_html("https://www.presidency.ucsb.edu/advanced-search?field-keywords=Europe&field-keywords2=&field-keywords3=&from%5Bdate%5D=&to%5Bdate%5D=&person2=200300&items_per_page=25")
searchresults %>% html_nodes("*") %>% html_attr("href")

searchresults %>% html_nodes(".views-field-title a") %>% html_attr("href")


# Do you notice something about the links? 
# They are missing a part. That is because they are relative links within the directory structure of the webpage. 
# To 'repair' them, we need to add the **base url** of the webpage. 
# This is typically just the url of the webpage we originally scraped from.

# to fix this issue we can use the paste0() function

urls <- searchresults %>% html_nodes(".views-field-title a") %>% html_attr("href")
urls <- paste0("https://www.presidency.ucsb.edu",urls)
urls

#### 5. What have we learned so far? ##############################

# We can extract tables
# We can extract content between HTML tags
# We can extract specific parts of a website
# We can extract links

# What do we do now?

#### 6. Automation ###############################################

##### 6.1 One page #########################################

# Now, we want to scrape multiple speeches on one summary page at once. What do we need to do?

# Individual steps

# 1. Parse the website
searchresults <- read_html("https://www.presidency.ucsb.edu/advanced-search?field-keywords=Europe&field-keywords2=&field-keywords3=&from%5Bdate%5D=&to%5Bdate%5D=&person2=200300&items_per_page=25")

# 2. Extract links from speeches we want to scrape
urls <- searchresults %>% html_nodes(".views-field-title a") %>% html_attr("href")

# 3. "Repair" links
urls <- paste0("https://www.presidency.ucsb.edu",urls)
urls

# 4. Extract part of speech that you want
speechtext <- read_html(urls[1]) %>% 
  html_nodes(".field-docs-content") %>%
  html_text()

date <- read_html(urls[1]) %>% 
  html_nodes(".date-display-single") %>%
  html_text()


# Question: which part will vary when you try to repeat this multiple times?

speech_df <- NULL

for (i in 1:25){
  text <- read_html(urls[i]) %>% 
    html_nodes(".field-docs-content") %>%
    html_text()
  date <- read_html(urls[i]) %>% 
    html_nodes(".date-display-single") %>%
    html_text()

  df_temp <- data.frame(speech = text, date = date)
  speech_df <- rbind(speech_df, df_temp)
}

speech_df

##### 6.2 Multiple pages #######################################
# Can you do this for multiple summary pages?
# Is there a logic? Click through the first few pages of results - what changes about the url as you do so?
browseURL("https://www.presidency.ucsb.edu/advanced-search?field-keywords=Europe&field-keywords2=&field-keywords3=&from%5Bdate%5D=&to%5Bdate%5D=&person2=200300&items_per_page=25&page=")

resulturls <- paste0("https://www.presidency.ucsb.edu/advanced-search?field-keywords=Europe&field-keywords2=&field-keywords3=&from%5Bdate%5D=&to%5Bdate%5D=&person2=200300&items_per_page=25&page=", 0:10)
resulturls

results_df <- NULL

for (i in 1:4){
  for (j in 1:25){
    summary_url <- resulturls[i]
    webpage <- read_html(summary_url)
    url_html <- html_nodes(webpage, ".views-field-title a")
    url_html <- html_attr(url_html,"href")

    url <- paste0("https://www.presidency.ucsb.edu", url_html[j])
    
    #Date
    date <- read_html(url) %>% 
      html_nodes(".date-display-single") %>%
      html_text()
    
    #Text
    speechtext <- read_html(url) %>% 
      html_nodes(".field-docs-content") %>%
      html_text()
    
    
    #Create a data frame
    df_temp <- data.frame(date = date, text = speechtext)
    results_df <- rbind(results_df, df_temp)
  }
}


