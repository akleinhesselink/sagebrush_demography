#### dbQueries 
questionMarks = function( vals ) { 
  x = length(vals)
  return( paste( rep("?", x), collapse = ',') ) 
}

makeExceptionalUpdateQuery = function ( x) { 
  paste( "UPDATE plants
         SET end_date = 
         ( 
           SELECT MAX(date) 
           FROM status 
           WHERE ID = plants.ID
           AND ID IN (", questionMarks( x) ,")
         )                    
         WHERE EXISTS 
         (
           SELECT date
           FROM status
           WHERE ID = plants.ID 
           AND ID IN (", questionMarks( x ), ")
         );")}


q.update.end_date = "UPDATE plants 
                      SET end_date = 
                      (
                      SELECT MAX(date)
                      FROM status
                      WHERE id = plants.id 
                      AND status = 0 
                      AND date(date) > date(?)
                      )                    
                      WHERE EXISTS 
                      (
                      SELECT date
                      FROM status
                      WHERE id = plants.id 
                      AND status = 0
                      AND date(date) > date(?)
                      );" 


q.update.active = "UPDATE plants 
                    SET active = 0 
                    WHERE end_date IS NOT NULL;"

