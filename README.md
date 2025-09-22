# Veridion Entity Resolution
Dragos Florea

# Entity Resolution

### General approach:

The dataset contains variations and inconsistencies in company and
location fields, including differences in spelling, abbreviation and
missing / incomplete information. Therefore, exact matching is often not
possible. Instead, I opted for the following approach:

1.  **Text standardization** to minimize differences.

2.  **Fuzzy matching** using the Jaro-Wickler similarity measure.

### Setting up the environment:

### Text processing:

The first step was to normalize the text. The following function was
created to:

- Convert all characters to uppercase.

- Remove punctuation.

- Trim leading and trailing whitespace.

  ``` r
  parse_text <- function(text) {
    text %>%
      str_to_upper() %>%
      str_remove_all("[:punct:]") %>%
      stri_trans_general("ASCII") %>%
      str_remove("^ | $")
  }
  ```

Entity types (e.g., *Ltd.*, *Inc.*) and terms related to location (e.g.,
*Municipality, District*) were often abbreviated or omitted in either
the input or output data. To handle this, I compiled a list of common
entity types and location terms. A function then removes or standardizes
these elements. For entity types, I included both a trailing space and
“\$” to later remove them only if they appear at the end of a string.

``` r
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
```

For the location data I defined the following additional function to
convert the values into a dataframe:

``` r
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
```

### Input/output preparation

The dataset was split into input and output subsets and the values were
standardized using the previously defined functions:

``` r
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
```

### Matching strategy

Matching is handled by calculating the Jaro-Wickler distances between
the following fields, adding the values together and selecting the
lowest one.

Lookup fields:

<table style="width:68%;">
<colgroup>
<col style="width: 30%" />
<col style="width: 37%" />
</colgroup>
<thead>
<tr>
<th>String in Input</th>
<th>String(s) in Output</th>
</tr>
</thead>
<tbody>
<tr>
<td>input_company_name</td>
<td>company_name<br />
company_legal_names<br />
company_commercial_names</td>
</tr>
<tr>
<td>input_main_city</td>
<td>main_city<br />
locations$city</td>
</tr>
<tr>
<td>input_main_region</td>
<td>main_region<br />
locations$region</td>
</tr>
<tr>
<td>input_main_postcode</td>
<td>main_postcode<br />
locations$postcode</td>
</tr>
</tbody>
</table>

**Country codes** are standardized and therefore compared directly for
equality.

Comparisons of region, city, and postcode are only attempted when
country codes match. Otherwise, the maximum distance (1) is assigned.
This approach prioritizes output records with the same country code.

``` r
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

print(stri_c("Example result for company ", input$input_company_name[2], ":"))
```

\[1\] “Example result for company 2OPERATE AS:”

``` r
df %>% filter(input_company_name %>% parse_text == input$input_company_name[2]) %>% select(company_name, company_legal_names, company_commercial_names, main_country_code, main_region, main_city, main_postcode, locations)
```

# A tibble: 5 × 8

company_name company_legal_names company_commercial_n…¹
main_country_code <chr> <chr> <chr> <chr>  
1 2operate 2operate A/S. 2operate DK  
2 CO 2 Operate CO2 Operate B.V. CO 2 Operate <NA>  
3 Conzentrate Conzentrate A/S. Conzentrate \| Conzent… DK  
4 2A Pharma ApS. 2A Pharma ApS. \| 2… <NA> DK  
5 RenSams Solutions RenSams Solutions … RenSams Solutions DK  
\# ℹ abbreviated name: ¹​company_commercial_names \# ℹ 4 more variables:
main_region <chr>, main_city <chr>, main_postcode <chr>, \# locations
<chr>

``` r
# The original data for input 2
company_matches[[2]]
```

                   [,1]      [,2]      [,3]      [,4]     [,5]

company_name_match 0 0.1325758 0.3181818 0.4351852 0.473832
country_code_match 0 0.0000001 0.0000000 0.0000000 0.000000 region_match
0 0.0000001 0.0000000 0.0000000 0.000000 city_match 0 0.0000001
0.0000000 0.0000000 0.000000 postcode_match 0 0.0000001 0.3333333
0.0000000 0.000000

``` r
# The scores
colSums(company_matches[[2]]) %>% which.min()
```

\[1\] 1

``` r
# Row with best (lowest) score

best_match_computed %>% as_tibble() %>% ggplot(aes(value)) + 
    geom_histogram(binwidth = 0.1, fill = "goldenrod2") +
    labs(x = "Similarity Score", y = "Entities") +
    ggtitle("Best (Lowest) Score for Each Search Engine Input:") + theme_minimal()
```

![](eexplenation_files/figure-commonmark/unnamed-chunk-6-1.png)

### Thresholding and manual review

To support quality control, a threshold can be applied to automatically
accept matches above a certain similarity level. Records falling below
the threshold are flagged for manual review.

A small UI was built to assist with this process. For each unmatched
entity, users can:

- Define a threshold.

- Review candidate matches in a simple table.

- Select the correct record via double-click, which stores the chosen
  row in a variable  
  (manually_selected_rows).

<img src="images/clipboard-2417630262.png" width="240" />

![](images/clipboard-3978708098.png)

``` r
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
```

### Final integration

The final dataset is produced by combining:

1.  Matches exceeding the similarity threshold.

2.  Manually reviewed and confirmed matches.

Non-matches are filtered out, and the cleaned, matched dataset is ready
for export.

``` r
manually_selected_rows <- readRDS(url("https://raw.githubusercontent.com/dragosff/Data-Analyst-Challenge/refs/heads/main/manually_selected_rows.rds"))
tresh_hold <- 0.5

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

matched_df
```

# A tibble: 543 × 76

input_row_key input_company_name input_main_country_c…¹
input_main_country <dbl> <chr> <chr> <chr>  
1 0 24-SEVEN MEDIA NETWO… PK Pakistan  
2 1 2OPERATE A/S DK Denmark  
3 10 ACCENTURE SERVICES AS NO Norway  
4 100 COMBA TELECOM LIMITED HK Hong Kong  
5 101 Comengineering Sdn. … MY Malaysia  
6 102 COMMSCOPE SOLUTIONS … SG Singapore  
7 103 CommScope Technologi… CH Switzerland  
8 105 COMVIVA TECHNOLOGIES… IN India  
9 106 CONSCIA DANMARK A/S DK Denmark  
10 107 CONTROL RISKS GROUP … GB United Kingdom  
\# ℹ 533 more rows \# ℹ abbreviated name: ¹​input_main_country_code \# ℹ
72 more variables: input_main_region <chr>, input_main_city <chr>, \#
input_main_postcode <chr>, input_main_street <chr>, \#
input_main_street_number <chr>, veridion_id <chr>, company_name <chr>,
\# company_legal_names <chr>, company_commercial_names <chr>, \#
main_country_code <chr>, main_country <chr>, main_region <chr>, …

``` r
# write_csv(matched_df, "output.csv")
```
