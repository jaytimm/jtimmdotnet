


# Read and collapse tooling text
tooling <- readLines("~/Dropbox/GitHub/blog/other/Q-researcher/inputs/jt-tooling.txt")
tooling_text <- paste(tooling, collapse = "\n")

# Compose the prompt
news_query_generator_system_prompt <- glue::glue('
# Search Generation Prompt for News Articles

## Research Profile
Use the following background to guide query generation:

------

{tooling_text}

------

## Search Generation Instructions
- Generate 7–10 search queries based on the research profile above.
- Include 2–3 queries based on current trends and new or hypothetical developments you\'re aware of.

**Guidelines:**
- Write clear, concise queries that resemble article headlines; avoid wordy or overly technical phrasing.
- Do **not** include time references (e.g., "latest", "recent", "2023").
- Focus on technical substance and practical use — think like researchers, developers, or technical users.
')





#### SCHEMA:
news_query_generator_schema <- ellmer::type_array(
  "A list of search queries for news, each with a topic-specific label.",
  items = ellmer::type_object(
    "One query for news retrieval, categorized by focus.",
    query = ellmer::type_string("The phrasing of the search query."),
    category = ellmer::type_string("The topic category (e.g. 'tools', 'application', 'technical', etc.)")
  )
)

#### FUNCTION:
news_query_generator_fn <- function() {
  json_input <- jsonlite::toJSON(list(), auto_unbox = TRUE, pretty = TRUE)
  
  ellmer::chat_openai(model = "gpt-4o", 
                      api_args = list(temperature = 1),
                      system_prompt = news_query_generator_system_prompt)$extract_data(
    #json_input, 
    '', #Focus on llamaindex',
    type = news_query_generator_schema
  )
}

#### TOOL:
news_query_generator <- list(
  name = "news_query_generator",
  description = "Generates 7-10 categorized, news-friendly search queries.",
  args = list(),
  system_prompt = news_query_generator_system_prompt,
  schema = news_query_generator_schema,
  fn = news_query_generator_fn
)

news_query_generator
