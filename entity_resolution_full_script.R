library(tidyverse)
library(stringdist)
library(stringi)

df <- read_csv("https://raw.githubusercontent.com/dragosff/Data-Analyst-Challenge/refs/heads/main/presales_data_sample.csv")
elf_codes <- read_csv("https://raw.githubusercontent.com/dragosff/Data-Analyst-Challenge/refs/heads/main/2023-09-28-elf-code-list-v1.5.csv") 

parse_text <- function(text) {
  text %>%
    str_to_upper() %>%
    str_remove_all("[:punct:]") %>%
    stri_trans_general("ASCII") %>%
    str_remove("^ | $")
}

common_substrings <- list(
  
  elf_code = str_c(
    " ", 
    elf_codes %>%
      select(`Abbreviations Local language`, `Abbreviations transliterated`) %>%
      unlist(use.names = F) %>%
      na.omit %>%
      sapply(function(e) str_split(e, ";")) %>% 
      unlist %>%
      parse_text %>%
      unique %>%
      append(c("PRIVATE LIMITED", "PVT LIMITED", 
               "CO LTD", "PTE LIMITED", "AB PUBL")) %>%
      as_tibble() %>% 
      mutate(nchar = nchar(value)) %>% 
      arrange(desc(nchar)) %>%
      pull(value), 
    "$"),
  
  city = df$main_city %>%
    parse_text %>%
    unique %>% 
    str_split(" ") %>%
    unlist %>%
    table %>% 
    as.data.frame %>%
    arrange(desc(Freq)) %>% 
    .[1:5,1] %>%
    as.character %>% 
    append("SUBDISTRICT", after = 1),
  
  region = df$main_region %>%
    parse_text %>%
    unique %>% 
    str_split(" ") %>%
    unlist %>%
    table %>% 
    as.data.frame() %>%
    arrange(desc(Freq)) %>% 
    .[1:5,1] %>%
    as.character
)

remove_common_string <- function(e, type) {
  e %>% 
    str_remove(str_c(common_substrings[[type]], collapse = "|")) %>% 
    str_remove("^ | $|  ")
}

parse_location <- function(e) {
  e %>% str_split(", ") %>%
    lapply(function(f) 
      f %>%
        na_if("") %>% 
        `names<-`(c("country_code", "country", "region", "city", "postcode",
                    "street", "street_number", "latitude", "longitude")) %>% 
        bind_rows() %>% 
        mutate(
          across(c(latitude, longitude), as.numeric),
          across(where(is.character), parse_text)
        )
    ) %>%
    list_rbind()
}

input <- df[,colnames(df)[str_which(colnames(df),"^input")]] %>% 
  unique() %>%
  mutate(across(where(is.character), parse_text))

cnt_companies <- nrow(input)

# Columns consisting of more then 1 value will be converted to lists
array_columns <- colnames(df)[
  !colnames(df) %in% c("num_locations","locations")
][
  str_which(
    colnames(df)[!colnames(df) %in% c("num_locations","locations")], 
    "s$"
  )
]

output <- lapply(input$input_row_key,  function(e)
  filter(df, input_row_key == e)[,str_which(colnames(df),"^input",TRUE)] %>%
    # Splitting columns containing an array into lists of character vectors
    mutate(
      locations = locations %>% 
        str_split(" \\| ") %>% 
        map(possibly(parse_location, otherwise = NA))
    ) %>%
    mutate(
      across(
        any_of(array_columns), function(f) 
          f %>% str_split(" \\| ") %>% lapply(parse_text)
      )
    ) %>%
    mutate(across(where(is.character), parse_text))
)

# Replacing missing values with 0.0000001, so that if two entries have 
#the same distance the one with more complete data will be selected.

na_substitute <- 0.0000001

company_matches <- lapply(
  1:cnt_companies, function(e) {
    
    country_code_match <- input$input_main_country_code[e] == output[[e]]$main_country_code
    
    cbind(
      company_name_match = rbind(
        stringdist(
          output[[e]]$company_name %>% remove_common_string("elf_code"), 
          input$input_company_name[e] %>% remove_common_string("elf_code"), 
          method = "jw"
        ),
        sapply(
          output[[e]]$company_commercial_names %>% 
            remove_common_string("elf_code"), function(f)
              min(
                stringdist(
                  f,
                  input$input_company_name[e] %>% remove_common_string("elf_code"),
                  method = "jw")
              ),
          USE.NAMES = F
        ),
        sapply(
          output[[e]]$company_legal_names %>% 
            remove_common_string("elf_code"), function(f)
              min(
                stringdist(
                  f,
                  input$input_company_name[e] %>% remove_common_string("elf_code"),
                  method = "jw")
              ),
          USE.NAMES = F
        )
      ) %>% 
        apply(2, min, na.rm = TRUE),
      country_code_match = as.integer(!country_code_match),
      region_match = ifelse(
        country_code_match,
        rbind(
          stringdist(
            output[[e]]$main_region %>% remove_common_string("region"),
            input$input_main_region[e] %>% remove_common_string("region"),
            method = "jw"),
          sapply(
            output[[e]]$locations %>% 
              map(
                possibly(function(x)
                  x %>%
                    filter(country_code == input$input_main_country_code[e]) %>%
                    pull(region),
                  otherwise = NA
                )
              ),
            function(f)
              min(
                stringdist(
                  f %>% remove_common_string("region"),
                  input$input_main_region[e] %>% remove_common_string("region"),
                  method = "jw"), 
                na.rm = TRUE
              )
          )
        ) %>% apply(2, na_if, Inf) %>% apply(2, min, na.rm = TRUE) %>% na_if(Inf),
        1
      ),
      city_match = ifelse(
        country_code_match,
        rbind(
          stringdist(
            output[[e]]$main_city %>% remove_common_string("city"),
            input$input_main_city[e] %>% remove_common_string("city"),
            method = "jw"),
          sapply(
            output[[e]]$locations %>% 
              map(
                possibly(
                  function(x) x %>%
                    filter(country_code == input$input_main_country_code[e]) %>%
                    pull(city),
                  otherwise = NA
                )
              ), 
            function(f)
              min(
                stringdist(
                  f %>% remove_common_string("city"),
                  input$input_main_city[e] %>% remove_common_string("city"),
                  method = "jw"), 
                na.rm = TRUE
              )
          )
        ) %>% apply(2, na_if, Inf) %>% apply(2, min, na.rm = TRUE) %>% na_if(Inf),
        1
      ),
      postcode_match = ifelse(
        country_code_match,
        rbind(
          stringdist(
            output[[e]]$main_postcode,
            input$input_main_postcode[e],
            method = "jw"
          ),
          sapply(
            output[[e]]$main_postcode %>% 
              map(
                possibly(
                  function(x) x %>%
                    filter(country_code == input$input_main_country_code[e]) %>%
                    pull(main_postcode),
                  otherwise = NA
                )
              ), 
            function(f)
              min(
                stringdist(
                  f,
                  input$input_main_postcode[e],
                  method = "jw"), 
                na.rm = TRUE
              )
          )
        ) %>% apply(2, na_if, Inf) %>% apply(2, min, na.rm = TRUE) %>% na_if(Inf),
        1
      )
    ) %>% apply(1, replace_na, na_substitute)
  })

best_match_computed <- sapply(1:cnt_companies, function(e) {
  sapply(lapply(company_matches, colSums, na.rm = TRUE), min)[e]
})

tt <- tktoplevel()
tcl_val_treshhold <- tclVar()
lbl_treshhold <- ttklabel(tt, text="Theshold:")
entry_treshhold <- tcltk::ttkentry(tt, textvariable = tcl_val_treshhold)
tkgrid(lbl_treshhold, entry_treshhold, padx = 5, pady = 5)
on_ok <- function() {
  tresh_hold <<- tclvalue(tcl_val_treshhold) %>% as.numeric()
  tkdestroy(tt)
}
btn_ok <- ttkbutton(tt, text = "OK", command = on_ok)
tkgrid(btn_ok, row = 1, column = 0, columnspan = 2, pady = 2)
tkwait.window(tt)

to_verify <- which(best_match_computed > tresh_hold)

manually_selected_rows <- c()

tt <- tktoplevel()
lookup_value_frame <- ttkframe(tt)
table_frame <- ttkframe(tt)
table_header_frame <- ttkframe(table_frame)
lookup_value_frame <- ttkframe(table_frame)
table_content_frame <- ttkframe(table_frame)
tkgrid(table_frame, row = 0, column = 0)
tkgrid(table_header_frame, row = 0, column = 0, `in` = table_frame)
tkgrid(lookup_value_frame, row = 1, column = 0, `in` = table_frame)
tkgrid(table_content_frame, row = 2, column = 0, `in` = table_frame)
n_rows <- 6

for(i in 1:length(to_verify)) {
  lookup_row <- input[to_verify[i],2:7] %>%
    as.data.frame() %>%   # tcltk will not accept tibble objects
    select(-input_main_country)
  
  # Rows to select for each unmatched entity. No-Match row also added
  skeleton_df <- output[[to_verify[i]]] %>% 
    select(
      company_name, main_country_code, main_region, main_city, main_postcode
    ) %>%
    bind_rows(
      c(
        company_name = "NO MATCH", main_country_code = "", 
        main_region = "", main_city = "", main_postcode = "")
    ) %>%
    as.data.frame()
  
  col_widths <- c(350, 50, 200, 150, 100)
  row_height <- 20
  
  n_cols <- ncol(skeleton_df)
  
  for (j in 1:n_cols) {
    lookup_value_cell <-  tkcanvas(
      lookup_value_frame, 
      width = col_widths[j], 
      height = 30, 
      background = "lightgray"
    )
    
    tkgrid(
      lookup_value_cell,
      row = 0, column = j-1,
      `in` = lookup_value_frame
    )
    
    table_header_canvas_cell <- tkcanvas(
      table_header_frame, 
      width = col_widths[j],
      height = 30,
      background = "black"
    )
    
    tkgrid( table_header_canvas_cell,
            row = 0, column = j-1,
            `in` = table_header_frame
    )
    
    tkcreate(
      lookup_value_cell,
      "text",
      5,
      row_height / 2 *1.5,
      anchor = "w",
      text = lookup_row[1,j],
      font = tkfont.create(family = "Courier", size = 10, weight = "bold")
    )
    
    tkcreate(
      table_header_canvas_cell,
      "text",
      5,
      row_height / 2 *1.5,
      anchor = "w",
      text = colnames(skeleton_df)[j],
      fill = "white",
      font = tkfont.create(family = "Courier", size = 10, weight = "bold")
    )
    
    for (k in 1:n_rows) {
      table_content_canvas_cell <- tkcanvas(
        table_content_frame, 
        width = col_widths[j], 
        height = 30, 
        background = "white"
      )
      
      tkgrid(
        table_content_canvas_cell,
        row = k-1, column = j-1,
        `in` = table_content_frame
      )
      
      tkcreate(
        table_content_canvas_cell,
        "text",
        5,
        row_height / 2 *1.5,
        anchor = "w",
        text = skeleton_df[k,j],
        font = tkfont.create(family = "Courier", size = 10)
      )
      
      clicked <- tclVar(0)
      
      local({
        row <- k
        tkbind(
          table_content_canvas_cell, "<Double-Button-1>", function() {
            manually_selected_rows[i] <<- row
            tclvalue(clicked) <- 1
          })
      })
    }
  }
  tkwm.protocol(tt, "WM_DELETE_WINDOW", function() {
    tclvalue(clicked) <- 1
    tkdestroy(tt)
  })
  tkwait.variable(clicked)
  print(skeleton_df[k,"company_name"])
}
tkdestroy(tt)

manually_selected_rows[manually_selected_rows==6] <- NA

best_match_combined <- sapply(1:cnt_companies, function(e) {
  sapply(lapply(company_matches, rowSums, na.rm = TRUE), which.min)[e]
})

best_match_combined[which(best_match_computed > tresh_hold)] <- manually_selected_rows

matched_df <- lapply(1:cnt_companies, function(e) {
  bind_cols(
    df %>%
      filter(input_row_key == input$input_row_key[e]) %>%
      .[best_match_combined[e],]
  )
}) %>% 
  list_rbind() %>%
  filter(!is.na(input_row_key))

matched_df %>%
  write_csv("output.csv")