#' Create a RAG service client
#'
#' @param .base_url RAG service base URL.
#' @param .api_key RAG service API key.
#' @param .session_id Stable RAG session ID. Generated when `NULL`.
#' @param .include_trace Include structured RAG execution traces by default.
#' @param .system_prompt System prompt used for chat requests.
#' @param .condense_prompt Condense prompt used for chat requests.
#' @param .context_prompt Context prompt used for chat requests.
#' @param .context_refine_prompt Context refine prompt used for chat requests.
#' @param .response_prompt Response prompt used for chat requests.
#' @param .citation_qa_template Citation QA prompt used for chat requests.
#' @param .citation_refine_template Citation refine prompt used for chat requests.
#'
#' @return An R6 RAG client.
#'
#' @details
#' The returned client keeps a stable service URL, API key, session ID, and
#' prompt set. Use `$ingest_url()`/`$ingest_pdf()` for blocking interactive
#' ingest, or `$ingest_url_async()`/`$ingest_pdf_async()` with `$wait_ingest()`
#' when polling manually. Use `$export_store()` to download Qdrant points,
#' including vectors by default, and `$import_store()` to restore an exported
#' point set.
#' Use `$export_chunks()` for parsed chunk text and `$export_documents()` for
#' chunks grouped back into parsed documents. All export and import operations
#' are restricted to the client's active session. Imported point and metadata
#' tenant IDs are replaced by the active session ID. Set `.include_trace=TRUE`
#' to receive query rewrites, retrieval results, filter decisions, scores,
#' effective prompts, citation validation, execution stages, and timings. This
#' trace does not expose hidden model chain-of-thought.
#'
#' @importFrom R6 R6Class
#' @importFrom curl curl_fetch_stream form_file handle_setheaders handle_setopt new_handle
#' @importFrom httr2 request req_body_json req_body_multipart req_headers req_perform resp_body_json resp_body_string resp_status
#' @importFrom vns extract_content_html
#' @export
#'
#' @examples
#' NULL
rag_client <- function(
  .base_url=rag_default_service_url(),
  .api_key=Sys.getenv("RAG_SERVICE_API_KEY", unset=""),
  .session_id=NULL,
  .include_trace=FALSE,
  .system_prompt=rag_default_prompts()$system_prompt,
  .condense_prompt=rag_default_prompts()$condense_prompt,
  .context_prompt=rag_default_prompts()$context_prompt,
  .context_refine_prompt=rag_default_prompts()$context_refine_prompt,
  .response_prompt=rag_default_prompts()$response_prompt,
  .citation_qa_template=rag_default_prompts()$citation_qa_template,
  .citation_refine_template=rag_default_prompts()$citation_refine_template
){

  RagClient$new(
    .base_url=.base_url,
    .api_key=.api_key,
    .session_id=.session_id,
    .include_trace=.include_trace,
    .system_prompt=.system_prompt,
    .condense_prompt=.condense_prompt,
    .context_prompt=.context_prompt,
    .context_refine_prompt=.context_refine_prompt,
    .response_prompt=.response_prompt,
    .citation_qa_template=.citation_qa_template,
    .citation_refine_template=.citation_refine_template
  )

}


#' Normalize RAG source payloads
#'
#' @param .sources Source payload returned by the RAG service.
#'
#' @return A list of source records.
#' @export
#'
#' @examples
#' NULL
rag_source_list <- function(.sources){

  if(is.null(.sources)){
    return(list())
  }
  if(is.data.frame(.sources)){
    return(lapply(seq_len(nrow(.sources)), function(.row_id){
      as.list(.sources[.row_id, , drop=FALSE])
    }))
  }
  if(is.list(.sources)){
    if(length(.sources) == 0){
      return(list())
    }
    if(!is.list(.sources[[1]]) && !is.null(names(.sources))){
      return(list(as.list(.sources)))
    }
    return(.sources)
  }

  list(as.list(.sources))

}


RagClient <- R6::R6Class(
  "RagClient",
  public=list(
    base_url=NULL,
    api_key=NULL,
    session_id=NULL,
    include_trace=FALSE,
    system_prompt=NULL,
    condense_prompt=NULL,
    context_prompt=NULL,
    context_refine_prompt=NULL,
    response_prompt=NULL,
    citation_qa_template=NULL,
    citation_refine_template=NULL,

    initialize=function(
      .base_url=rag_default_service_url(),
      .api_key=Sys.getenv("RAG_SERVICE_API_KEY", unset=""),
      .session_id=NULL,
      .include_trace=FALSE,
      .system_prompt=rag_default_prompts()$system_prompt,
      .condense_prompt=rag_default_prompts()$condense_prompt,
      .context_prompt=rag_default_prompts()$context_prompt,
      .context_refine_prompt=rag_default_prompts()$context_refine_prompt,
      .response_prompt=rag_default_prompts()$response_prompt,
      .citation_qa_template=rag_default_prompts()$citation_qa_template,
      .citation_refine_template=rag_default_prompts()$citation_refine_template
    ){

      self$base_url <- sub("/+$", "", .base_url)
      self$api_key <- .api_key
      self$session_id <- rag_coalesce(.session_id, private$new_session_id())
      self$include_trace <- .include_trace
      self$system_prompt <- .system_prompt
      self$condense_prompt <- .condense_prompt
      self$context_prompt <- .context_prompt
      self$context_refine_prompt <- .context_refine_prompt
      self$response_prompt <- .response_prompt
      self$citation_qa_template <- .citation_qa_template
      self$citation_refine_template <- .citation_refine_template

      invisible(self)

    },

    reset_session=function(.session_id=NULL){

      self$session_id <- rag_coalesce(.session_id, private$new_session_id())
      invisible(self)

    },

    request=function(.path){

      private$base_req(.path)

    },

    ingest_pdf_async=function(.pdf_paths, .label=NULL, .filenames=NULL){

      stopifnot(length(.pdf_paths) > 0)

      .parts <- vector("list", length(.pdf_paths))
      names(.parts) <- rep("files", length(.pdf_paths))
      for(.pdf_id in seq_along(.pdf_paths)){
        .filename <- if(!is.null(.filenames) && length(.filenames) >= .pdf_id){
          .filenames[[.pdf_id]]
        } else {
          basename(.pdf_paths[[.pdf_id]])
        }
        .parts[[.pdf_id]] <- curl::form_file(
          .pdf_paths[[.pdf_id]],
          type="application/pdf",
          name=.filename
        )
      }
      if(!is.null(.label) && nzchar(.label)){
        .parts[["labels"]] <- .label
      }

      .response <- private$base_req("/ingest/async") |>
        httr2::req_body_multipart(!!!.parts) |>
        httr2::req_perform()
      private$check_response(.response, "Ingest start failed")

      httr2::resp_body_json(.response)$job_id %||% stop("No job_id returned", call.=FALSE)

    },

    ingest_url_async=function(.url, .label=NULL){

      .html <- self$clean_html(.url)
      self$ingest_html_async(
        .html=.html,
        .name=private$url_filename(.url),
        .label=.label
      )

    },

    ingest_url=function(
      .url,
      .label=NULL,
      .timeout_s=1200,
      .interval_s=1
    ){

      .job_id <- self$ingest_url_async(.url, .label=.label)
      self$wait_ingest(
        .job_id,
        .timeout_s=.timeout_s,
        .interval_s=.interval_s
      )

    },

    ingest_html_async=function(.html, .name="page.html", .label=NULL){

      .payload <- list(
        docs=list(list(
          name=.name,
          content=.html,
          label=if(!is.null(.label) && nzchar(.label)) .label else .name
        ))
      )

      .response <- private$base_req("/ingest/urls/async") |>
        httr2::req_body_json(.payload) |>
        httr2::req_perform()
      private$check_response(.response, "URL ingest start failed")

      httr2::resp_body_json(.response)$job_id %||% stop("No job_id returned", call.=FALSE)

    },

    ingest_pdf=function(
      .pdf_paths,
      .label=NULL,
      .filenames=NULL,
      .timeout_s=1200,
      .interval_s=1
    ){

      .job_id <- self$ingest_pdf_async(
        .pdf_paths,
        .label=.label,
        .filenames=.filenames
      )
      self$wait_ingest(
        .job_id,
        .timeout_s=.timeout_s,
        .interval_s=.interval_s
      )

    },

    poll_ingest=function(.job_id){

      .response <- private$base_req(stringi::stri_c("/ingest/status/", .job_id)) |>
        httr2::req_perform()
      private$check_response(.response, "Status failed")

      httr2::resp_body_json(.response)

    },

    wait_ingest=function(
      .job_id,
      .timeout_s=1200,
      .interval_s=1
    ){

      .started_at <- Sys.time()
      repeat {
        .status <- self$poll_ingest(.job_id)
        if(.status$status %in% c("succeeded", "failed")){
          if(identical(.status$status, "failed")){
            stop(
              "Ingest failed: ",
              .status$error %||% .status$message %||% "unknown error",
              call.=FALSE
            )
          }
          return(.status)
        }

        .elapsed_s <- as.numeric(difftime(Sys.time(), .started_at, units="secs"))
        if(.elapsed_s >= .timeout_s){
          stop("Timed out waiting for ingest job: ", .job_id, call.=FALSE)
        }
        Sys.sleep(.interval_s)
      }

    },

    delete_documents=function(.label){

      .response <- private$base_req("/chat/delete") |>
        httr2::req_body_json(list(label=.label)) |>
        httr2::req_perform()
      private$check_response(.response, "L\u00f6schen fehlgeschlagen")

      invisible(TRUE)

    },

    list_documents=function(){

      .response <- private$base_req("/chat/export") |>
        httr2::req_body_json(list(include_vectors=FALSE)) |>
        httr2::req_perform()
      private$check_response(.response, "Dokumentenliste fehlgeschlagen")

      .data <- httr2::resp_body_json(.response)
      .points <- .data$points %||% list()
      .docs <- lapply(.points, function(.point){
        .payload <- .point$payload %||% list()
        list(
          label=.payload$source_label %||% .payload$source_file %||% "Unbenannt",
          source_file=.payload$source_file %||% "Unbekannt"
        )
      })

      if(length(.docs) == 0){
        return(list())
      }

      .doc_table <- table(vapply(.docs, function(.doc){
        .doc$label
      }, character(1)))

      lapply(names(.doc_table), function(.label){
        list(label=.label, count=as.integer(.doc_table[[.label]]))
      })

    },

    export_store=function(
      .path=NULL,
      .include_vectors=TRUE,
      .pretty=TRUE
    ){

      .response <- private$base_req("/chat/export") |>
        httr2::req_body_json(list(
          include_vectors=.include_vectors
        )) |>
        httr2::req_perform()
      private$check_response(.response, "Knowledge store export failed")

      .store <- httr2::resp_body_json(.response, simplifyVector=FALSE)

      if(!is.null(.path)){
        jsonlite::write_json(
          .store,
          .path,
          auto_unbox=TRUE,
          null="null",
          pretty=.pretty
        )
      }

      .store

    },

    import_store=function(
      .store=NULL,
      .path=NULL,
      .append=TRUE,
      .distance="cosine"
    ){

      if(is.null(.store) && is.null(.path)){
        stop("Provide either .store or .path.", call.=FALSE)
      }
      if(!is.null(.store) && !is.null(.path)){
        stop("Provide only one of .store or .path.", call.=FALSE)
      }

      if(!is.null(.path)){
        .store <- jsonlite::read_json(.path, simplifyVector=FALSE)
      }

      .points <- private$store_points(.store)
      private$validate_import_points(.points, .distance)

      .response <- private$base_req("/chat/import") |>
        httr2::req_body_json(list(
          points=.points,
          append=.append,
          distance=.distance
        )) |>
        httr2::req_perform()
      private$check_response(.response, "Knowledge store import failed")

      httr2::resp_body_json(.response, simplifyVector=FALSE)

    },

    export_chunks=function(
      .path=NULL,
      .include_metadata=TRUE,
      .pretty=TRUE
    ){

      .response <- private$base_req("/chat/export/chunks") |>
        httr2::req_body_json(list(
          include_metadata=.include_metadata
        )) |>
        httr2::req_perform()
      private$check_response(.response, "Parsed chunk export failed")

      .chunks <- httr2::resp_body_json(.response, simplifyVector=FALSE)

      if(!is.null(.path)){
        jsonlite::write_json(
          .chunks,
          .path,
          auto_unbox=TRUE,
          null="null",
          pretty=.pretty
        )
      }

      .chunks

    },

    export_documents=function(
      .path=NULL,
      .include_metadata=TRUE,
      .include_chunks=TRUE,
      .collapse="\n\n",
      .pretty=TRUE
    ){

      .chunk_export <- self$export_chunks(
        .include_metadata=.include_metadata
      )
      .documents <- private$chunks_to_documents(
        .chunk_export$chunks %||% list(),
        .include_chunks=.include_chunks,
        .collapse=.collapse
      )
      .document_export <- list(
        collection=.chunk_export$collection,
        count=length(.documents),
        documents=.documents
      )

      if(!is.null(.path)){
        jsonlite::write_json(
          .document_export,
          .path,
          auto_unbox=TRUE,
          null="null",
          pretty=.pretty
        )
      }

      .document_export

    },

    clean_html=function(.url){

      .raw_html <- httr2::request(.url) |>
        httr2::req_perform() |>
        httr2::resp_body_string()

      .readable_html <- tryCatch(
        vns::extract_content_html(.raw_html),
        error=function(.error) NULL
      )
      if(!is.null(.readable_html) && length(.readable_html) > 0 && nzchar(.readable_html[[1]])){
        return(.readable_html[[1]])
      }

      private$fallback_article_html(.raw_html)

    },

    chat_stream=function(
      .message,
      .history=list(),
      .include_trace=NULL,
      .system_prompt=NULL,
      .condense_prompt=NULL,
      .context_prompt=NULL,
      .context_refine_prompt=NULL,
      .response_prompt=NULL,
      .citation_qa_template=NULL,
      .citation_refine_template=NULL,
      .on_token=NULL
    ){

      .body <- jsonlite::toJSON(
        private$chat_payload(
          .message=.message,
          .history=.history,
          .include_trace=.include_trace,
          .system_prompt=.system_prompt,
          .condense_prompt=.condense_prompt,
          .context_prompt=.context_prompt,
          .context_refine_prompt=.context_refine_prompt,
          .response_prompt=.response_prompt,
          .citation_qa_template=.citation_qa_template,
          .citation_refine_template=.citation_refine_template
        ),
        auto_unbox=TRUE,
        null="null"
      )

      .headers <- list(
        "Content-Type"="application/json",
        "X-Session-Id"=self$session_id
      )
      if(nzchar(self$api_key)){
        .headers[["X-Api-Key"]] <- self$api_key
      }

      .accumulator <- raw(0)
      .answer <- ""
      .sources <- NULL
      .prompts <- NULL
      .trace <- NULL

      .handle <- curl::new_handle()
      curl::handle_setheaders(.handle, .list=.headers)
      curl::handle_setopt(.handle, postfields=.body)

      .process_line <- function(.line){
        if(!nzchar(.line)){
          return(invisible(NULL))
        }

        .chunk <- tryCatch(
          jsonlite::fromJSON(.line, simplifyVector=FALSE),
          error=function(.error) NULL
        )
        if(is.null(.chunk)){
          return(invisible(NULL))
        }

        if(!is.null(.chunk$type) && .chunk$type == "token"){
          .answer <<- stringi::stri_c(.answer, .chunk$delta %||% "")
          if(!is.null(.on_token)){
            .on_token(.chunk$delta %||% "")
          }
        } else if(!is.null(.chunk$type) && .chunk$type == "sources"){
          .sources <<- rag_source_list(.chunk$sources)
        } else if(!is.null(.chunk$type) && .chunk$type == "trace"){
          .trace <<- .chunk$trace
        } else if(!is.null(.chunk$type) && .chunk$type == "done"){
          if(!is.null(.chunk$answer)){
            .answer <<- .chunk$answer
          }
          .sources <<- rag_source_list(.chunk$sources)
          .prompts <<- .chunk$prompts
          .trace <<- .chunk$trace %||% .trace
        } else if(!is.null(.chunk$type) && .chunk$type == "error"){
          stop("Stream error: ", .chunk$error, call.=FALSE)
        }

        invisible(NULL)
      }

      curl::curl_fetch_stream(
        stringi::stri_c(self$base_url, "/chat/stream"),
        function(.chunk){
          .accumulator <<- c(.accumulator, .chunk)
          repeat {
            .line_end <- match(as.raw(10), .accumulator, nomatch=0)
            if(.line_end == 0){
              break
            }
            .line <- rawToChar(.accumulator[seq_len(.line_end - 1)])
            .accumulator <<- if(.line_end < length(.accumulator)){
              .accumulator[(.line_end + 1):length(.accumulator)]
            } else {
              raw(0)
            }
            .process_line(.line)
          }
        },
        handle=.handle
      )
      if(length(.accumulator) > 0){
        .process_line(rawToChar(.accumulator))
      }

      list(
        answer=.answer,
        sources=rag_source_list(.sources),
        prompts=.prompts,
        trace=.trace
      )

    },

    chat_stream_config=function(
      .message,
      .history=list(),
      .include_api_key=TRUE,
      .include_trace=NULL,
      .system_prompt=NULL,
      .condense_prompt=NULL,
      .context_prompt=NULL,
      .context_refine_prompt=NULL,
      .response_prompt=NULL,
      .citation_qa_template=NULL,
      .citation_refine_template=NULL
    ){

      .config <- c(
        list(
          base_url=self$base_url,
          session_id=self$session_id,
          api_key=if(isTRUE(.include_api_key) && nzchar(self$api_key)) self$api_key else NULL
        ),
        private$chat_payload(
          .message=.message,
          .history=.history,
          .include_trace=.include_trace,
          .system_prompt=.system_prompt,
          .condense_prompt=.condense_prompt,
          .context_prompt=.context_prompt,
          .context_refine_prompt=.context_refine_prompt,
          .response_prompt=.response_prompt,
          .citation_qa_template=.citation_qa_template,
          .citation_refine_template=.citation_refine_template
        )
      )

      .config

    }
  ),
  private=list(
    new_session_id=function(){

      stringi::stri_c(
        "rag-",
        as.integer(Sys.time()),
        "-",
        sample.int(100000, 1)
      )

    },

    base_req=function(.path){

      .path <- if(startsWith(.path, "/")) .path else stringi::stri_c("/", .path)
      .request <- httr2::request(stringi::stri_c(self$base_url, .path)) |>
        httr2::req_headers(`X-Session-Id`=self$session_id)
      if(nzchar(self$api_key)){
        .request <- .request |>
          httr2::req_headers(`X-Api-Key`=self$api_key)
      }

      .request

    },

    check_response=function(.response, .message){

      if(httr2::resp_status(.response) >= 300){
        stop(
          .message,
          ": ",
          httr2::resp_body_string(.response),
          call.=FALSE
        )
      }

      invisible(.response)

    },

    fallback_article_html=function(.raw_html){

      .doc <- xml2::read_html(.raw_html)
      xml2::xml_remove(xml2::xml_find_all(
        .doc,
        ".//script|.//style|.//noscript|.//nav|.//header|.//footer"
      ))
      .candidates <- xml2::xml_find_all(.doc, ".//article|.//main")
      .node <- if(length(.candidates) > 0){
        .candidates[[which.max(nchar(xml2::xml_text(.candidates)))]]
      } else {
        xml2::xml_find_first(.doc, ".//body")
      }
      if(inherits(.node, "xml_missing")){
        .node <- xml2::xml_find_first(.doc, ".")
      }

      .text <- xml2::xml_text(.node)
      .text <- stringi::stri_replace_all_regex(.text, "\\s+", " ")
      .text <- stringi::stri_trim_both(.text)

      stringi::stri_c(
        "<html><body><article><p>",
        htmltools::htmlEscape(.text),
        "</p></article></body></html>"
      )

    },

    url_filename=function(.url){

      .url_path <- strsplit(.url, "[?#]")[[1]][[1]]
      .name <- basename(.url_path)
      if(!nzchar(.name) || .name %in% c("/", ".")){
        .name <- "page.html"
      }
      if(!endsWith(tolower(.name), ".html")){
        .name <- stringi::stri_c(.name, ".html")
      }

      .name

    },

    store_points=function(.store){

      if(!is.null(.store$points)){
        return(.store$points)
      }
      if(is.list(.store) && length(.store) > 0 && !is.null(.store[[1]]$id)){
        return(.store)
      }

      stop("Store must be an export_store() result or a list of points.", call.=FALSE)

    },

    validate_import_points=function(.points, .distance){

      if(!.distance %in% c("cosine", "euclid", "dot")){
        stop(".distance must be one of 'cosine', 'euclid', or 'dot'.", call.=FALSE)
      }
      if(length(.points) == 0){
        stop("No points found to import.", call.=FALSE)
      }

      .missing_vectors <- vapply(.points, function(.point){
        is.null(.point$vector) || length(.point$vector) == 0
      }, logical(1))
      if(any(.missing_vectors)){
        stop(
          "Import requires vectors. Export the store with .include_vectors=TRUE.",
          call.=FALSE
        )
      }

      invisible(TRUE)

    },

    chunks_to_documents=function(.chunks, .include_chunks=TRUE, .collapse="\n\n"){

      if(length(.chunks) == 0){
        return(list())
      }

      .groups <- split(.chunks, vapply(.chunks, private$document_key, character(1)))
      unname(lapply(.groups, function(.group){
        .first <- .group[[1]]
        .texts <- vapply(.group, function(.chunk){
          private$safe_chr(.chunk$text)
        }, character(1))

        .document <- list(
          tenant_id=.first$tenant_id,
          source_label=.first$source_label,
          source_file=.first$source_file,
          chunk_count=length(.group),
          chunk_ids=vapply(.group, function(.chunk){
            private$safe_chr(.chunk$id)
          }, character(1)),
          page_numbers=private$chunk_pages(.group),
          headings=private$chunk_headings(.group),
          text=stringi::stri_c(.texts[nzchar(.texts)], collapse=.collapse)
        )
        if(isTRUE(.include_chunks)){
          .document$chunks <- .group
        }

        .document
      }))

    },

    document_key=function(.chunk){

      stringi::stri_c(
        private$safe_chr(.chunk$tenant_id),
        private$safe_chr(.chunk$source_label),
        private$safe_chr(.chunk$source_file),
        sep="\r"
      )

    },

    safe_chr=function(.x){

      if(is.null(.x) || length(.x) == 0){
        return("")
      }

      .value <- .x[[1]]
      if(is.null(.value) || length(.value) == 0){
        return("")
      }
      .value <- .value[[1]]
      if(is.na(.value)){
        return("")
      }

      as.character(.value)

    },

    chunk_pages=function(.chunks){

      .pages <- unlist(lapply(.chunks, function(.chunk){
        .chunk$page_numbers %||% list()
      }), use.names=FALSE)
      if(length(.pages) == 0){
        return(NULL)
      }

      sort(unique(as.integer(.pages)))

    },

    chunk_headings=function(.chunks){

      .headings <- unlist(lapply(.chunks, function(.chunk){
        .chunk$headings %||% list()
      }), use.names=FALSE)
      .headings <- as.character(.headings)
      .headings <- unique(.headings[!is.na(.headings) & nzchar(.headings)])
      if(length(.headings) == 0){
        return(NULL)
      }

      .headings

    },

    chat_payload=function(
      .message,
      .history=list(),
      .include_trace=NULL,
      .system_prompt=NULL,
      .condense_prompt=NULL,
      .context_prompt=NULL,
      .context_refine_prompt=NULL,
      .response_prompt=NULL,
      .citation_qa_template=NULL,
      .citation_refine_template=NULL
    ){

      list(
        message=.message,
        history=.history,
        include_trace=rag_coalesce(.include_trace, self$include_trace),
        system_prompt=rag_coalesce(.system_prompt, self$system_prompt),
        condense_prompt=rag_coalesce(.condense_prompt, self$condense_prompt),
        context_prompt=rag_coalesce(.context_prompt, self$context_prompt),
        context_refine_prompt=rag_coalesce(.context_refine_prompt, self$context_refine_prompt),
        response_prompt=rag_coalesce(.response_prompt, self$response_prompt),
        citation_qa_template=rag_coalesce(.citation_qa_template, self$citation_qa_template),
        citation_refine_template=rag_coalesce(.citation_refine_template, self$citation_refine_template)
      )

    }
  )
)


rag_coalesce <- function(.x, .y){

  if(is.null(.x)) .y else .x

}


`%||%` <- rag_coalesce


rag_default_service_url <- function(){

  if(file.exists("/.dockerenv")){
    return("http://nd_services-rag_service:9126")
  }

  "https://rag_service.dsjlu.wirtschaft.uni-giessen.de"

}


rag_default_prompts <- function(){

  list(
    system_prompt="Du bist ein Retrieval-Assistant und beantwortest Fragen zu den hochgeladenen Dokumenten der Nutzenden.\n\nRegeln:\n- Nutze nur den bereitgestellten Kontext; erfinde keine Zitate.\n- F\u00fcr jede Hauptaussage (z. B. Beitr\u00e4ge) f\u00fcge Zitiermarken wie [1], [2] ein.\n- Jede Zitiermarke muss zu einer der zur\u00fcckgegebenen Quellen passen.",
    condense_prompt="Gegeben sind der bisherige Chat-Verlauf und eine Nachfrage. Formuliere daraus eine eigenst\u00e4ndige Frage.\n\nChat-Verlauf:\n{chat_history}\nNachfrage: {question}\nEigenst\u00e4ndige Frage:",
    context_prompt="Dies ist ein freundliches Gespr\u00e4ch zwischen Nutzenden und einer KI. Die KI antwortet ausf\u00fchrlich und mit vielen Details aus dem Kontext. Wenn sie etwas nicht wei\u00df, sagt sie das ehrlich.\n\nHier sind die relevanten Dokumente f\u00fcr den Kontext:\n\n{context_str}\n\nAnweisung: Formuliere auf Basis dieser Dokumente eine detaillierte Antwort auf die folgende Frage. Wenn es im Kontext nicht steht, antworte mit \u201eWei\u00df ich nicht.\u201c",
    response_prompt="Du bist ein RAG-Assistent. Antworte nur auf Grundlage des Kontexts.\n\nRegeln:\n- F\u00fcge inline-Zitiermarken wie [1], [2] ein, die zu den Quellen passen.\n- Keine separate Quellenliste ausgeben.\n- Antworte knapp und strukturiert auf Deutsch.",
    context_refine_prompt="Dies ist ein freundliches Gespr\u00e4ch zwischen Nutzenden und einer KI. Die KI antwortet ausf\u00fchrlich und mit vielen Details aus dem Kontext. Wenn sie etwas nicht wei\u00df, sagt sie das ehrlich.\n\nHier sind die relevanten Dokumente:\n\n{context_msg}\n\nBestehende Antwort:\n{existing_answer}\n\nAnweisung: Verfeinere die bestehende Antwort mithilfe des Kontexts. Wenn der Kontext nicht hilft, wiederhole die bestehende Antwort unver\u00e4ndert.",
    citation_qa_template="Bitte beantworte auf Deutsch ausschlie\u00dflich auf Basis der nummerierten Quellen und f\u00fcge Inline-Zitate wie [1], [2] ein. Gib keine separate Quellenliste aus.\n------\n{context_str}\n------\nFrage: {query_str}\nAntwort:",
    citation_refine_template="Bitte beantworte auf Deutsch ausschlie\u00dflich auf Basis der nummerierten Quellen und f\u00fcge Inline-Zitate wie [1], [2] ein. Gib keine separate Quellenliste aus. Wenn die Quellen nicht helfen, wiederhole die bestehende Antwort.\n------\n{context_msg}\n------\nFrage: {query_str}\nBestehende Antwort: {existing_answer}\nVerfeinerte Antwort:"
  )

}
