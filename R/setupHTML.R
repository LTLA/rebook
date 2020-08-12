#' Set up HTML elements
#'
#' Set up Javascript and CSS elements for each chapter,
#' primarily for the custom collapsible class.
#'
#' @return Prints HTML to standard output set up JS and CSS elements.
#'
#' @details
#' The custom collapsible class allows us to hide details until requested by the user.
#' This improves readability by reducing the clutter in the compiled chapter.
#'
#' @author Aaron Lun
#' 
#' @examples
#' setupHTML()
#'
#' @seealso
#' \code{\link{chapterPreamble}}, which calls this function.
#'
#' \code{\link{extractCached}} and \code{\link{prettySessionInfo}}, which use the custom collapsible class.
#'
#' @inherit prettySessionInfo examples
#' @export
setupHTML <- function() {
    cat('<script>
document.addEventListener("click", function (event) {
    if (event.target.classList.contains("rebook-collapse")) {
        event.target.classList.toggle("active");
        var content = event.target.nextElementSibling;
        if (content.style.display === "block") {
            content.style.display = "none";
        } else {
            content.style.display = "block";
        }
    }
})
</script>

<style>
.rebook-collapse {
  background-color: #eee;
  color: #444;
  cursor: pointer;
  padding: 18px;
  width: 100%;
  border: none;
  text-align: left;
  outline: none;
  font-size: 15px;
}

.rebook-content {
  padding: 0 18px;
  display: none;
  overflow: hidden;
  background-color: #f1f1f1;
}
</style>')
}
