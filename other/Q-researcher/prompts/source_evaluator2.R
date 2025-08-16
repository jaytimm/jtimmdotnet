

# Read and collapse tooling text
tooling <- readLines("~/Dropbox/GitHub/blog/other/Q-researcher/inputs/jt-tooling.txt")
tooling_text <- paste(tooling, collapse = "\n")


# Combine into final prompt
source_eval_profile_prompt2 <- glue::glue('
# Article Evaluator Prompt
## Context
You are evaluating sources to determine their relevance to specific research interests. Score each source based on how well it matches the research profile and filter out low-signal content.

## Research Profile
{tooling_text}

## Evaluation Task
For each source, provide:
1. **Relevance Score** (0-10): How well does this match the research interests?
2. **Key Concepts** (1-3): Main technical concepts covered
3. **Recommendation** (include/exclude): Should this be included in the digest?

## Technical Depth Guidelines:
- **High depth (8-10)**: Implementation details, novel technical approaches, specific architectural choices, experimental setups, performance metrics, reproducible methods
- **Medium depth (5-7)**: Solid methodology, some technical specificity, but limited novelty or unclear practical applications
- **Low depth (0-4)**: High-level concepts only, preliminary findings, poor methodology, purely descriptive

## Evaluation Guidelines:
- Score 8-10: Directly relevant with high technical value
- Score 5-7: Somewhat relevant, moderate technical depth
- Score 0-4: Low relevance or shallow content
- Always exclude basic tutorials and pure marketing content
- Prioritize sources with implementation details or novel applications
', .open = '{', .close = '}')


#### SCHEMA:
source_evaluator_schema2 <- ellmer::type_array(
  "A list of evaluation results, each containing a score, key insights, and recommendation for a single source.",
  items = ellmer::type_object(
    "Evaluation result for one document.",
    text_id = ellmer::type_string("The ID of the source document."),
    score = ellmer::type_integer("A 0–10 relevance, evidence, or ideation score depending on the mode."),
    key_points = ellmer::type_array(
      "A list of 1–3 key concepts, evidence, or insights depending on the evaluation mode.",
      items = ellmer::type_string("A short phrase or sentence highlighting a key element of the source.")
    ),
    recommendation = ellmer::type_string("Recommendation tag based on mode (e.g., include/exclude, high/medium/low, explore/consider/skip).")
  )
)


#### FUNCTION:
source_evaluator_fn2 <- function(input_text) {

  ellmer::chat_openai(
    model = "gpt-4o",
    system_prompt = source_evaluator_schema2
  )$extract_data(input_text, 
                 type = source_evaluator_schema2)
}



#### TOOL:
source_evaluator2 <- list(
  name = "source_evaluator",
  description = paste(
    "Evaluates source documents.",
    "Returns: text_id, score (0–10), key_points (1–3), and recommendation label.",
    sep = "\n"
  ),
  args = list(input_data = "data.frame"),
  system_prompt = source_eval_profile_prompt2,
  schema = source_evaluator_schema2,
  fn = source_evaluator_fn2
)

source_evaluator2


