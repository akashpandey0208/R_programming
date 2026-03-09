data_table_server <- function(id, selected_dataset){

moduleServer(id, function(input, output, session){

ns <- session$ns

message("Table generator server started")

# =============================
# Dataset metadata
# =============================
dataset_info <- reactive({

req(selected_dataset())

df <- selected_dataset()

data.frame(
column = names(df),
type = sapply(df, function(x) class(x)[1]),
stringsAsFactors = FALSE
)

})


# =============================
# Group By UI
# =============================
output$group_by_ui <- renderUI({

req(dataset_info())

categorical <- dataset_info()$column[
dataset_info()$type %in% c("character","factor")
]

selectInput(
ns("group_by"),
"Group Data By",
choices = c("NONE", categorical)
)

})


# =============================
# Group variable reactive
# =============================
group_var <- reactive({

if(is.null(input$group_by) || input$group_by == "NONE"){
NULL
}else{
input$group_by
}

})


# =============================
# Master variable list
# =============================
selected_vars <- reactiveVal(list())

observeEvent(selected_dataset(),{
selected_vars(list())
})


# =============================
# Drop variable
# =============================
observeEvent(input$drop_area_dropped_var, {

vars <- selected_vars()

new_entry <- list(
df = "dataset",
var = input$drop_area_dropped_var,
stat = "DESCRIPTIVE"
)

vars[[length(vars)+1]] <- new_entry

selected_vars(vars)

})


# =============================
# Variables list
# =============================
output$variables_list <- renderUI({

req(dataset_info())

vars <- dataset_info()$column

tagList(
lapply(vars,function(v){

tags$div(
class="drag-var",
draggable="true",
`data-var`=v,
v
)

})
)

})


# =============================
# Stat map
# =============================
stat_map <- list(
numeric = c("DESCRIPTIVE"),
categorical = c("COUNT","FREQ")
)


# =============================
# Selected variables UI
# =============================
output$selected_vars_ui <- renderUI({

vars <- selected_vars()

if(length(vars)==0) return(NULL)

current_cols <- dataset_info()$column

vars <- Filter(function(x) x$var %in% current_cols, vars)

tagList(

lapply(seq_along(vars), function(i){

var_name <- vars[[i]]$var
stat_val <- vars[[i]]$stat

type <- dataset_info()$type[
dataset_info()$column == var_name
]

stats <- if(type %in% c("numeric","integer")){
stat_map$numeric
}else{
stat_map$categorical
}

fluidRow(

column(4,strong(var_name)),

column(
6,
selectInput(
ns(paste0("stat_",i)),
NULL,
choices=stats,
selected=stat_val
)
),

column(
2,
actionButton(
ns(paste0("remove_",var_name)),
NULL,
icon=icon("times"),
class="remove-btn"
)
)

)

})

)

})


# =============================
# Update stat
# =============================
observe({

vars <- selected_vars()

for(i in seq_along(vars)){

local({

idx <- i

observeEvent(input[[paste0("stat_",idx)]],{

vars <- selected_vars()

vars[[idx]]$stat <- input[[paste0("stat_",idx)]]

selected_vars(vars)

},ignoreInit=TRUE)

})

}

})


# =============================
# Remove variable
# =============================
observe({

vars <- selected_vars()

for(v in vars){

local({

var_name <- v$var

observeEvent(input[[paste0("remove_",var_name)]],{

vars <- selected_vars()

vars <- Filter(function(x) x$var != var_name, vars)

selected_vars(vars)

},ignoreInit=TRUE)

})

}

})


# =============================
# Numeric descriptive stats
# =============================
calc_numeric_stats <- function(df,var,group=NULL){

if(is.null(group)){

x <- df[[var]]

data.frame(
Statistic=c("n","Mean (SD)","Median","Q1 | Q3","Min | Max"),
Value=c(
sum(!is.na(x)),
paste0(round(mean(x,na.rm=TRUE),2)," (",round(sd(x,na.rm=TRUE),2),")"),
round(median(x,na.rm=TRUE),2),
paste0(round(quantile(x,0.25,na.rm=TRUE),2)," | ",round(quantile(x,0.75,na.rm=TRUE),2)),
paste0(round(min(x,na.rm=TRUE),2)," | ",round(max(x,na.rm=TRUE),2))
)
)

}else{

df |>
dplyr::group_by(.data[[group]]) |>
dplyr::summarise(
n=sum(!is.na(.data[[var]])),
Mean=round(mean(.data[[var]],na.rm=TRUE),2),
SD=round(sd(.data[[var]],na.rm=TRUE),2),
.groups="drop"
) |>
dplyr::mutate(
`Mean (SD)`=paste0(Mean," (",SD,")")
) |>
dplyr::select(
!!group,
n,
`Mean (SD)`
)

}

}


# =============================
# Operation tables UI
# =============================
output$operation_tables <- renderUI({

req(selected_vars())

vars <- selected_vars()

tagList(

lapply(seq_along(vars),function(i){

v <- vars[[i]]

div(
style="margin-bottom:30px",

h4(paste(v$stat,"(",v$var,")")),

DT::DTOutput(ns(paste0("operation_",i)))
)

})

)

})


# =============================
# Render operations
# =============================
observe({

req(selected_vars())

vars <- selected_vars()

for(i in seq_along(vars)){

local({

idx <- i

output[[paste0("operation_",idx)]] <- DT::renderDT({

entry <- selected_vars()[[idx]]

df <- selected_dataset()

var <- entry$var
stat <- entry$stat


if(stat=="DESCRIPTIVE"){

result <- calc_numeric_stats(df,var,group_var())

}else if(stat=="COUNT"){

if(is.null(group_var())){

result <- df |>
dplyr::count(.data[[var]]) |>
dplyr::rename(Value=!!rlang::sym(var),Count=n)

}else{

result <- df |>
dplyr::count(.data[[group_var()]],.data[[var]])

}

}else if(stat=="FREQ"){

if(is.null(group_var())){

result <- df |>
dplyr::count(.data[[var]]) |>
dplyr::mutate(Percent=round(n/sum(n)*100,2)) |>
dplyr::rename(Value=!!rlang::sym(var))

}else{

result <- df |>
dplyr::count(.data[[group_var()]],.data[[var]]) |>
dplyr::group_by(.data[[group_var()]]) |>
dplyr::mutate(Percent=round(n/sum(n)*100,2)) |>
dplyr::ungroup()

}

}else{

result <- data.frame(Result="Unsupported stat")

}

DT::datatable(
  result,
  rownames = FALSE,
  filter = "top",
  options = list(
    pageLength = 5,
    scrollX = TRUE,
    scrollY = "300px",
    scrollCollapse = TRUE,
    ordering = TRUE,
    lengthChange = FALSE,
    autoWidth = TRUE
  )
)

})

})

}

})


# =============================
# Master table for download
# =============================
master_table <- reactive({

req(selected_vars())
req(selected_dataset())

df <- selected_dataset()
vars <- selected_vars()

tables <- list()

for(v in vars){

var <- v$var
stat <- v$stat

if(stat=="COUNT"){

tables[[var]] <- df |> dplyr::count(.data[[var]])

}

if(stat=="FREQ"){

tables[[var]] <- df |>
dplyr::count(.data[[var]]) |>
dplyr::mutate(percent=n/sum(n)*100)

}

if(stat=="DESCRIPTIVE"){

tables[[var]] <- calc_numeric_stats(df,var)

}

}

tables

})


# =============================
# Download table
# =============================
output$download_table <- downloadHandler(

filename=function(){

paste0("table_output.",tolower(input$download_type))

},

content=function(file){

tables <- master_table()

if(input$download_type=="CSV"){

combined <- dplyr::bind_rows(tables,.id="Variable")

write.csv(combined,file,row.names=FALSE)

}

if(input$download_type=="HTML"){

html <- ""

for(name in names(tables)){

html <- paste0(
html,
"<h3>",name,"</h3>",
knitr::kable(tables[[name]],format="html")
)

}

writeLines(html,file)

}

if(input$download_type=="RTF"){

library(rtf)

rtf_file <- RTF(file)

addParagraph(rtf_file,input$table_title)

for(name in names(tables)){

addParagraph(rtf_file,name)

addTable(rtf_file,tables[[name]])

}

done(rtf_file)

}

}

)


# =============================
# Download R code
# =============================
output$download_code <- downloadHandler(

filename=function(){
"reproduce_table.R"
},

content=function(file){

vars <- selected_vars()

code <- c(
"library(dplyr)",
"",
"df <- your_dataset",
""
)

for(v in vars){

var <- v$var
stat <- v$stat

if(stat=="COUNT"){

code <- c(code,paste0("df |> count(",var,")"))

}

if(stat=="FREQ"){

code <- c(code,paste0("df |> count(",var,") |> mutate(percent=n/sum(n)*100)"))

}

if(stat=="DESCRIPTIVE"){

code <- c(code,paste0("summary(df$",var,")"))

}

code <- c(code,"")

}

writeLines(code,file)

}

)

})
}