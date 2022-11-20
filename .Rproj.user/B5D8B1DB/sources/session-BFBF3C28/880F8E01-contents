library(htmltools)
library(tidyverse)
library(reactable)
library(mapboxer)
library(reactablefmtr)
library(crosstalk)

load("database.RData")

#------------------------ Data ------------------------------------
sample <- airbnb %>% drop_na(beds, review_scores_value) %>%
  mutate(property_type = as_factor(property_type),
         across(c("price"), parse_number)) %>%
  arrange(desc(review_scores_value)) %>% 
  mutate(score = row_number())

  

#------------------------ Theme ------------------------------------
htmltools::tags$link(href = "https://fonts.googleapis.com/css?family=Roboto:400,500&display=fallback", rel = "stylesheet")

#------ Color Variables ----------
primary <- "#FF5A5F"
text_color <- "#484848"
text_color_light <- "#767676"
text_color_lighter <- "#d7dce1"
bg_color <- "white"
gray <- "#eee"

airbnb_theme <- function() {
  search_icon <- function(fill = "none") {
    # Icon from https://boxicons.com
    svg <- sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" style="fill: rgba(255, 90, 95, 1);transform: rotate(90deg);msFilter:progid:DXImageTransform.Microsoft.BasicImage(rotation=1);"><path d="M10 18a7.952 7.952 0 0 0 4.897-1.688l4.396 4.396 1.414-1.414-4.396-4.396A7.952 7.952 0 0 0 18 10c0-4.411-3.589-8-8-8s-8 3.589-8 8 3.589 8 8 8zm0-14c3.309 0 6 2.691 6 6s-2.691 6-6 6-6-2.691-6-6 2.691-6 6-6z"></path></svg>', fill)
    sprintf("url('data:image/svg+xml;charset=utf-8,%s')", URLencode(svg))
  }
  reactableTheme(
    color = text_color,
    backgroundColor = bg_color,
    borderColor = gray,
    borderWidth = "1px",
    highlightColor = text_color_lighter,
    cellPadding = "15px 10px",
    cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"),
    #------ CSS ----------
    style = list(
      fontFamily = "Roboto",
      fontSize = "0.9rem",
      "a" = list(
        color = text_color,
        textDecoration = "none",
        "&:hover, &:focus" = list(
          textDecoration = "underline",
          textDecorationThickness = "1px"
        )
      ),
      "img" = list(borderRadius = "15px",
                   border = "5px solid #fff"),
      ".number" = list(
        color = text_color_light,
        fontFamily = "'Roboto', Helvetica, Arial, sans-serif;"
      ),
      #--------- Rank column ----------
      ".rank" = list(
        fontWeight = "600",
        color = "black",
        fontSize = "0.95rem"
      ),
      #--------- Superhost tag ----------
      ".tag" = list(
        padding = "0.125rem 0.25rem",
        color = "black",
        backgroundColor = "white",
        fontSize = "0.65rem",
        border = "1px solid black",
        borderRadius = "2px",
        textTransform = "uppercase"
      ),
      ".table-header" = list(
        #		background = "hsla(0, 0%, 60%, 0.1)"
      ),
      #--------- Sorting Style ----------
      '.table-header[aria-sort="ascending"]' = list(
        boxShadow = "inset 0 10px 0 -6px #FF5A5F"
      ),
      '.table-header[aria-sort="descending"]'= list(
        boxShadow = "inset 0 -10px 0 -6px #FF5A5F"	
      ),
      ".sorted" = list(
        backgroundColor = "hsla(0, 0%, 60%, 0.1)"
      ),
      #--------- Row Details ----------
      ".row-detail" = list(
        padding = "24px",
        boxShadow = "inset 0 1px 3px #dbdbdb",
        background = "hsla(0, 0%, 60%, 0.1)"
      ),
      ".detail-label" = list(
        margin = "1.25rem 0 0.25rem",
        fontSize = "0.875rem",
        paddingBottom = "10px",
        color = text_color_light
      ),
      ".detail-header" = list(
        marginBottom = "1rem",
        fontSize = "1.25rem",
        fontWeight = "600"
      ),
      ".detail-title" = list(
        marginLeft = "1rem",
        fontSize = "0.9rem",
        fontWeight = "400",
        color = text_color_light
      ),
      ".detail-description" = list(
        fontSize= "0.875rem"
      )
    ),
    #-------- Header -------------
    headerStyle = list(
      color = text_color_light,
      borderColor = primary,
      borderWidth = "5px",
      fontWeight = 400,
      fontSize = "0.7rem",
      letterSpacing = "1px",
      borderTopLeftRadius = "15%",
      borderTopRightRadius = "15%", 
      textTransform = "uppercase",
      "&:hover, &:focus" = list(color = text_color)
    ),
    #------- Search Bar -----------
    searchInputStyle = list(
      paddingLeft = "1.9rem",
      paddingTop = "0.5rem",
      paddingBottom = "0.5rem",
      width = "100%",
    #  border = "none",
      borderRadius = "25px",
      backgroundColor = bg_color,
      backgroundImage = search_icon(text_color_light),
      backgroundSize = "1rem",
      backgroundPosition = "left 0.5rem center",
      backgroundRepeat = "no-repeat",
      "&:focus" = list(backgroundColor = "rgba(255, 255, 255, 0.1)"),
      "&:hover, &:focus" = list(backgroundImage = search_icon(text_color)),
      "::placeholder" = list(color = text_color_lighter),
      "&:hover::placeholder, &:focus::placeholder" = list(color = text_color)
    ),
    #----------------- Pagination -----------------------------
    paginationStyle = list(color = text_color_light),
    pageButtonHoverStyle = list(backgroundColor = text_color_lighter),
    pageButtonActiveStyle = list(backgroundColor = text_color_lighter)
  )
}

#------------------------ Table ------------------------------------
reactable(
  sample %>% select(
    score, picture_url, name, neighbourhood_cleansed, review_scores_value,price, 
    property_type, accommodates, host_is_superhost, host_identity_verified,
    bedrooms, beds, bathrooms = bathrooms_text, 
    host_response_rate, host_response_time
    ),
  elementId = "airbnb",
  minRows = 10,
  defaultColDef = colDef(
    align = "center",
    headerClass = "table-header",
    class = JS("function(rowInfo, column, state) {
               // Highlight sorted columns
               for (let i = 0; i < state.sorted.length; i++) {
               if (state.sorted[i].id === column.id) {
               return 'sorted'}
               }}")),
	columns = list(
	  score = colDef(
	    class = "rank",
	    header = tagList(span("#", "aria-hidden" = "true", title = "Position")),
	    minWidth = 40,
	    align = "center",
	  ),
	  picture_url = colDef(
	    sticky = "left",
	    name = '',
	    minWidth = 150,
	    style = list(borderRight = "1px solid #eee"),
	    cell = embed_img(height = 100, width = 120)
	  ), 
	  name = colDef(
	    align = "left",
	    name = "Name",
	    style = list(textTransform = "capitalize"),
	    resizable = TRUE,
	    html = TRUE,
	    minWidth = 110,
	    cell = function(value, index) {
	      # Render as a link
	      url <- sprintf("https://www.airbnb.com/rooms/%s", sample[index, "id"])
	      title = tags$a(href = url, target = "_blank", as.character(value))
	      
	      if (sample[index, "host_is_superhost"] == 'TRUE') {
	          superhost_tag <- div(style = list(float = "center", marginTop = "5px"), 
	                               span(class = "tag", "Superhost"))
	          title <- tagList(title, superhost_tag)
	      }
	        title
	      }
	  ),
	  neighbourhood_cleansed = colDef(
	    name = "City",
	    minWidth = 90
	  ),
	  review_scores_value = colDef(
	    name = "Score",
	    minWidth = 90,
	    style = list(fontWeight = "bold", fontSize=16),
	    cell = icon_sets(sample, icons = "star", 
	                     colors =  text_color,
	                     icon_position = "left")
	  ),
	  price = colDef(
	    name = "Price",
	    minWidth = 70,
	    html = TRUE,
	    format = colFormat(suffix = " $"),
	    style = list(fontWeight = "bold", fontSize=16),
	    header = function(value) {
	      units <- div(style = "color: #DBDBDB; font-size: 12px; text-transform: lowercase;", 
	                   "night")
	      div(title = value, value, units)},
	  ),
	  accommodates = colDef(
	    name = "Guests",
	    minWidth = 80,
	    cell = icon_assign(sample, icon = "user", 
	                       fill_color = text_color,
	                       align_icons = "center")
	    
	  ),
	  bedrooms = colDef(
	    name = "Bedroom",
	    minWidth = 100,
	    header = function(value) {
	      units <- div(style = "color: #DBDBDB", "Beds")
	      div(title = value, value, units)
	    },
	    # Show beds under bedrooms
	    cell = function(value, index) {
	      if (sample$beds[index] ==1 ) {
	        bed <- " bed"
	      } else {bed <- " beds"}
	      div(
	        div(style = list(fontWeight = 600), value),
	        div(style = list(fontSize = "0.75rem",
	                         color = text_color_light), 
	            sample$beds[index], bed)
	      )
	    }
	  ),
	  host_response_rate = colDef(
	    name = "Response rate",
	    align = 'center', 
	    maxWidth = 120,
	    format = colFormat(percent = TRUE, digits = 1),
	    cell = function(value, index) {
	      div(
	        div(style = list(fontWeight = 600), value),
	        div(style = list(fontSize = "0.75rem",
	                         color = text_color_light), 
	            sample$host_response_time[index])
	      )
	    }
	  ),
	  bathrooms = colDef(minWidth = 110),
	  property_type = colDef(name = "Type"),
	  beds = colDef(show = F),
	  host_response_time = colDef(show = F),
	  host_identity_verified = colDef(show = F),
	  host_is_superhost = colDef(show = F)
  ),
  language = reactableLang(
    searchPlaceholder = "Search for accomodation",
    noData = "No Airbnb found",
    pageInfo = "{rowStart}\u2013{rowEnd} of {rows} tracks",
    pagePrevious = "\u276e",
    pageNext = "\u276f"
  ),
  theme = airbnb_theme(),
  showSortIcon = FALSE,
  highlight = TRUE,
  striped = TRUE,
  outlined = TRUE,
  searchable = TRUE,
  compact = F,
  wrap = TRUE,
  paginationType = "simple",
  defaultExpanded = FALSE,
  details = function(index) {
    # Render html in description text
    html <- function(x, inline = FALSE) {
      container <- if (inline) htmltools::span else htmltools::div
      container(dangerouslySetInnerHTML = list("__html" = x))
    }
    # Get the index of the row
    sample <- sample[index, ]
    #Define a new section in the description with a class
    section <- function(name, ...) {
      if (any(is.na(...))) NULL
      else tagList(div(class = "detail-label", name), ...)
    }
    
    detail <- div(
      class = "row-detail",
      div(class = "detail-header", sample$name, span(class = "detail-title", sample$neighbourhood_cleansed)),
      section("Description", html(sample$description)), br(),
      span(class = "detail-label", "Number of reviews: "), sample$number_of_reviews, br(),
      span(class = "detail-label", "Minimum nights: "), sample$minimum_nights, br(),
      span(class = "detail-label", "Maximum nights: "), sample$maximum_nights, br(),
      # Host Section
      h3("About the Host"),
      div(tags$a(href = sample$host_picture_url, target = "_blank", 
                 tags$img(src = sample$host_picture_url,
                          style="height:90px;width:90px; 
                                 border-radius: 50%; border: 0;
                                 float: left;  margin-right: 15px; padding-bottom: 10px;")),
          span(class = "detail-label", "Host Name: "), strong(sample$host_name), br(),
          span(class = "detail-label", "Host Since: "), str_to_title(format(as.Date(sample$host_since), "%e %B %Y")), br(),
          span(class = "detail-label", "Host Location: "), sample$host_location, br(),
          span(class = "detail-label", "Response Rate: "), sample$host_response_rate," · ", em(sample$host_response_time), br(),
          span(class = "detail-label", "Acceptance Rate: "), sample$host_acceptance_rate
      ),
      section("Amenities", gsub('\\[|\\]|\\"', "", sample$amenities)), br(),
      section("Map", " "),
      mapboxer(style = basemaps$Mapbox$light_v10, width = "100%", height = "200", 
               token = "pk.eyJ1IjoiYWxlY3Jpc3VjbSIsImEiOiJjbDVmbHJ3NDgxMmtqM2RwamM2NjNyZjcyIn0.73OTdsgbQo0u4q7_dDI0Iw") %>% 
        add_navigation_control() %>% 
        set_view_state(sample$longitude, sample$latitude, zoom = 14) %>%
        add_marker(sample$longitude, sample$latitude, popup=sample$name)
    )
    detail
  }
) -> table



#------------------------ Output ------------------------------------
div(style = "font-family: 'Roboto', Helvetica, Arial, sans-serif;",
    div(
      img(src = "Airbnb.png", style = "width:15%; padding-bottom: 15px; margin-right: 5px;"),
      h1("best accomodations in Ragusa, Sicily", style = "display: inline;")
    ),
    table) -> out


html_print(out)

