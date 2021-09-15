---- 1) Which are the top 5 authors with the most citations (from other papers). Return author names and number of citations.


MATCH (ar1:Article)-[r:CITES]->(ar2:Article),(ar2)-[:WRITTEN_BY]->(au:Author)
RETURN au.name AS Author, COUNT(r) AS Number_of_Citations
ORDER BY Number_of_Citations 
DESC LIMIT 5 


---- 2) Which are the top 5 authors with the most collaborations (with different authors). Return author names and number of collaborations.


MATCH(ar:Article)-[:WRITTEN_BY]->(au:Author),(ar)-[:WRITTEN_BY]->(au2:Author)
WHERE au.name<>au2.name
RETURN au.name AS Author, COUNT(DISTINCT au2) AS Number_of_Collaborations
ORDER BY Number_of_Collaborations 
DESC LIMIT 5


---- 3) Which is the author who has wrote the most papers without collaborations. Return author name and number of papers.


MATCH (ar:Article)-[w:WRITTEN_BY]->(au:Author)
MATCH (ar)-[w2:WRITTEN_BY]->(au2:Author)
WITH au, COUNT(ar) as  Number_of_Papers, COUNT(DISTINCT au2) as Collaborators_counter
WHERE Collaborators_counter = 1
RETURN au.name AS Author, Number_of_Papers
ORDER BY Number_of_Papers DESC
LIMIT 1


---- 4) Which author published the most papers in 2001? Return author name and number of papers. 


MATCH (ar:Article)-[w:WRITTEN_BY]->(au:Author), (ar)-[p:PUBLISHED_IN]->(j:Journal)
WHERE ar.year = '2001'
RETURN au.name AS Author, COUNT(ar) AS Number_of_Papers
ORDER BY Number_of_Papers 
DESC LIMIT 1


---- 5) Which is the journal with the most papers about “gravity” (derived only from the paper title) in 1998. Return name of journal and number of papers.


MATCH (ar:Article)-[p:PUBLISHED_IN]->(j:Journal)
WHERE ar.year ='1998' AND toLower(ar.title)CONTAINS "gravity"
RETURN j.journal AS Journal, COUNT(ar) AS Number_of_Papers
ORDER BY Number_of_Papers 
DESC LIMIT 1


---- 6) Which are the top 5 papers with the most citations? Return paper title and number of citations.


MATCH (ar1:Article)-[r:CITES]->(ar2:Article)
RETURN ar2.title as Paper_Title, COUNT(r) AS Number_of_Citations
ORDER BY Number_of_Citations DESC
LIMIT 5


---- 7) Which were the papers that use “holography” and “anti de sitter” (derived only from the paper abstract). Return authors and title.


MATCH (ar:Article)-[w:WRITTEN_BY]->(au:Author)
WHERE toLower(ar.abstract) CONTAINS "holography" 
AND toLower(ar.abstract) CONTAINS "anti de sitter"
RETURN au.name as Author, ar.title as Title



---- 8) Find the shortest path between ‘C.N. Pope’ and ‘M. Schweda’ authors (use any type of edges).
----    Return the path and the length of the path. Comment about the type of nodes and edges of the path.


MATCH p = shortestPath((au:Author{name:'C.N. Pope'})-[*]-(au2:Author{name:'M. Schweda'}))
RETURN [n in nodes(p)] as Path, length(p) as Path_Length


---- 9) Run again the previous query (8) but now use only edges between authors and papers. 
----    Comment about the type of nodes and edges of the path. Compare the results with query 8.


MATCH p = shortestPath((au:Author{name:'C.N. Pope'})-[w:WRITTEN_BY*]-(au2:Author{name:'M. Schweda'}))
RETURN [n in nodes(p)] as Path, length(p) as Path_Length


---- 10) Find all authors with shortest path lengths > 25 from author ‘Edward Witten’.
----     The shortest paths will be calculated only on edges between authors and articles. 
----     Return author name, the length and the paper titles for each path.
 

MATCH p = ShortestPath((au:Author{name:'Edward Witten'})-[w:WRITTEN_BY*]-(au2:Author))
WHERE au<>au2
AND length(p) > 25
AND NONE(n in nodes(p) WHERE n:Journal)
RETURN au2.name as Author_Name, length(p) AS Path_Length, [n in nodes(p) WHERE n.title IS NOT NULL| n.title] AS Paper_Title

