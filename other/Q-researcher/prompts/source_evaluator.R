# source_evaluator.


#### PROMPTS: 

source_eval_qna_prompt <- '
# Query-Focused Source Evaluator
## Context
You are evaluating sources to determine how well they answer a specific research question. Score each source based on relevance and confidence in addressing the query.

## Research Question
{research_question}

## Evaluation Task
For each source, provide:
1. **Relevance Score** (0-10): How directly does this address the research question?
2. **Key Evidence** (1-3): Main findings or data points that support answering the question
3. **Recommendation** (high/medium/low): Confidence this helps answer the question

## Answer Quality Guidelines:
- **High quality (8-10)**: Directly addresses question with robust methodology, clear results, sufficient sample sizes, reproducible findings
- **Medium quality (5-7)**: Partially addresses question with acceptable methodology, some limitations in scope or rigor
- **Low quality (0-4)**: Tangentially related, poor methodology, insufficient evidence, or purely speculative

## Evaluation Guidelines:
- Score 8-10: Direct answer with high-confidence evidence
- Score 5-7: Partial answer or moderate confidence in findings
- Score 0-4: Minimal relevance or unreliable evidence
- Prioritize sources with clear methodology and measurable outcomes
- Exclude sources that don\'t contribute to answering the specific question
'

source_eval_ideation_prompt <- '
# Research Ideation Evaluator
## Context
You are evaluating sources for their potential to inspire new research directions and novel approaches. Score based on creativity, transferability, and question-generating potential.

## Research Domain
{research_domain}

## Evaluation Task
For each source, provide:
  1. **Ideation Score** (0-10): How much potential does this have to spark new research ideas?
  2. **Key Insights** (1-3): Novel methods, unexpected findings, or cross-domain connections
3. **Recommendation** (explore/consider/skip): Potential for generating new research directions

## Ideation Value Guidelines:
- **High value (8-10)**: Novel methodological approaches, unexpected findings that challenge assumptions, creative cross-domain applications, methods transferable to other fields
- **Medium value (5-7)**: Interesting approaches with some novelty, moderate transferability, generates some new questions
- **Low value (0-4)**: Conventional approaches, expected results, limited cross-domain potential

## Evaluation Guidelines:
- Score 8-10: Highly novel approaches or surprising findings with broad applicability
- Score 5-7: Some creative elements or moderate cross-domain potential
- Score 0-4: Conventional work with limited inspirational value
- Prioritize methodological creativity and unexpected results over direct relevance
- Look for bridging concepts that connect disparate research areas
- Value sources that generate new questions rather than just answering existing ones
'



#### SCHEMA:
source_evaluator_schema <- ellmer::type_array(
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
source_evaluator_fn <- function(retrieved_contexts,
                                rhetorical_situation) {

  library(jsonlite)
  mode <- rhetorical_situation$retrieval_mode
  
  prompt_template <- switch(mode,
                            qna = source_eval_qna_prompt,
                            ideation = source_eval_ideation_prompt)

  injected_prompt <- gsub("\\{[^}]+\\}", 
                          rhetorical_situation$queries[1], 
                          prompt_template)
  
  input_list <- list(
    sources = lapply(seq_len(nrow(retrieved_contexts)), function(i) {
      list(
        text_id = retrieved_contexts$text_id[i],
        text = retrieved_contexts$chunk[i]
      )
    })
  )
  
  json_input <- jsonlite::toJSON(input_list, 
                                 auto_unbox = TRUE, 
                                 pretty = TRUE)
  
  ellmer::chat_openai(
    model = "gpt-4o",
    system_prompt = injected_prompt
  )$extract_data(json_input, 
                 type = source_evaluator_schema)
}






#### TOOL:
source_evaluator <- list(
  name = "source_evaluator",
  description = paste(
    "Evaluates source documents using one of three independent evaluation prompts.",
    "- 'profile': match to a research profile (include/exclude)",
    "- 'question': ability to answer a research question (high/medium/low)",
    "- 'ideation': potential to inspire new ideas (explore/consider/skip)",
    "Returns: text_id, score (0–10), key_points (1–3), and recommendation label.",
    sep = "\n"
  ),
  args = list(
    mode = "character",
    retrieved_contexts = "data.table",
    rhetorical_situation = "list"
  ),
  system_prompt = "dynamic_by_mode",
  schema = source_evaluator_schema,
  fn = source_evaluator_fn
)

source_evaluator
