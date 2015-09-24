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


q.update.end_date <- "UPDATE plants 
                      SET end_date = 
                        ( 
                          SELECT MIN(date)
                          FROM status 
                          WHERE ID = plants.ID
                          AND status = 0 
                        )
                      WHERE EXISTS 
                        (
                          SELECT date
                          FROM status
                          WHERE ID = plants.ID 
                          AND status = 0
                        );"

q.update.active = "UPDATE plants 
                    SET active = 0 
                    WHERE end_date IS NOT NULL;"

q.reborn = "SELECT ID, date, field_tag, ch, status, notes 
            FROM status 
            WHERE ID IN 
            (
              SELECT ID 
              FROM 
              (
                SELECT ID, max(date)
                FROM status 
                WHERE NOT status = 1
                AND date < 
                (
                  SELECT max(date)
                  FROM status AS max_status
                  WHERE max_status.ID = status.ID
                  AND max_status.status = 1
                )
                GROUP BY ID
              )
            )
            ORDER BY ID, date;"

q.update.reborn = "UPDATE status 
                    SET status = 1 
                    WHERE rowid IN 
                    (
                      SELECT rowid 
                      FROM 
                      (
                        SELECT rowid, max(date)
                        FROM status 
                        WHERE NOT status = 1
                        AND date < 
                        (
                          SELECT max(date)
                          FROM status AS max_status
                          WHERE max_status.ID = status.ID
                          AND max_status.status = 1
                        )
                      GROUP BY ID
                      )
                    );"

q.update.now.dead = "UPDATE status 
                SET status = 0 
                WHERE rowid IN 
                (
                  SELECT rowid 
                  FROM 
                  (
                    SELECT rowid 
                    FROM status 
                    WHERE NOT status = 1 
                    AND date < 
                    (
                      SELECT max(date) 
                      FROM status AS last_status
                      WHERE last_status.ID = status.ID
                      AND last_status.status = 0
                    )
                  )
                );"


q.now.dead = "SELECT ID, date, field_tag, ch, stem_d1, status, notes, rowid
              FROM status 
              WHERE ID IN 
                (
                  SELECT ID 
                  FROM 
                  (
                    SELECT ID, max(date)
                    FROM status 
                    WHERE status IN (2,3)
                    AND date <                   
                    (
                      SELECT max(date)
                      FROM status AS last_status
                      WHERE last_status.ID = status.ID
                      AND last_status.status = 0
                    )
                    GROUP BY ID
                  )
                )
            ORDER BY ID, date;"
