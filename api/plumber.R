#' @param id The unique identifier of the player(s).
#' @serializer unboxedJSON
#' @get /players
function(id){
    if (missing(id)) {
        dbGetQuery(pool, "SELECT * FROM mls.players ORDER BY player_id")
    } else {
        split_ids <- strsplit(as.character(id), "\\s*,\\s*")
        sql_ids <- paste0("('", paste0(split_ids[[1]], collapse = "', '"), "')")

        sql <- "SELECT * FROM mls.players WHERE player_id IN ?id ORDER BY player_id"
        query <- sqlInterpolate(pool, sql, id = SQL(sql_ids))
        dbGetQuery(pool, query)
    }
}
