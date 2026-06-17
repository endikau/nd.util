\name{rag_client}
\alias{rag_client}
\title{Create a RAG service client}
\usage{
rag_client(
  .base_url = rag_default_service_url(),
  .api_key = Sys.getenv("RAG_SERVICE_API_KEY", unset = ""),
  .session_id = NULL,
  .system_prompt = rag_default_prompts()$system_prompt,
  .condense_prompt = rag_default_prompts()$condense_prompt,
  .context_prompt = rag_default_prompts()$context_prompt,
  .context_refine_prompt = rag_default_prompts()$context_refine_prompt,
  .response_prompt = rag_default_prompts()$response_prompt,
  .citation_qa_template = rag_default_prompts()$citation_qa_template,
  .citation_refine_template = rag_default_prompts()$citation_refine_template
)
}
\arguments{
\item{.base_url}{RAG service base URL.}
\item{.api_key}{RAG service API key.}
\item{.session_id}{Stable RAG session ID. Generated when \code{NULL}.}
\item{.system_prompt}{System prompt used for chat requests.}
\item{.condense_prompt}{Condense prompt used for chat requests.}
\item{.context_prompt}{Context prompt used for chat requests.}
\item{.context_refine_prompt}{Context refine prompt used for chat requests.}
\item{.response_prompt}{Response prompt used for chat requests.}
\item{.citation_qa_template}{Citation QA prompt used for chat requests.}
\item{.citation_refine_template}{Citation refine prompt used for chat requests.}
}
\value{
An R6 RAG client.
}
\description{
Create a RAG service client.
}
\details{
The returned client keeps a stable service URL, API key, session ID, and
prompt set. Use \code{$export_store()} to download Qdrant points, including
vectors by default, and \code{$import_store()} to restore an exported point set.
Use \code{$export_chunks()} for parsed chunk text and \code{$export_documents()}
for chunks grouped back into parsed documents. Passing \code{.tenant_id = NULL}
exports the full collection; pass \code{.tenant_id = rag$session_id} to export
only the current session.
}
\examples{
NULL
}
