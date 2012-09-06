<?php
/* When asked for 

     http://www.haptonstahl.org/R/projectname/subfolder/file.R
   
   it will return the contents of
   
     https://raw.github.com/shaptonstahl/R/master/projectname/subfolder/file.R
   
   This file should be located in the document root of 
   haptonstahl.org.  A .htaccess file is required, poinitng
   404 errors to this file.
*/

$request_url = 'http://' . $_SERVER['HTTP_HOST'] . $_SERVER['REQUEST_URI'];
$proper_request_stem = 'http://www.haptonstahl.org/R';

if( false != stristr($request_url, $proper_request_stem) )
{
  // build the URL
  $url = str_replace($proper_request_stem, 'https://raw.github.com/shaptonstahl/R/master', $request_url);
  
  // $response = file_get_contents($url);
  
  // make the request
  $ch = curl_init();
  curl_setopt($ch, CURLOPT_URL, $url);

  // Set so curl_exec returns the result instead of outputting it.
  curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

  // Get the response and close the channel.
  $response = curl_exec($ch);
  curl_close($ch);

  // Send header for a text file.
  header('Content-type: text/plain');

  // If file not retrieved, echo a specific, message in R code.
  // Othewise, echo file contents already retrieved.
  if( false == $result )
  {
    $response = 'cat("Unable to retrieve\n\n  ' .
      $request_url . '\n\n' .
      'You might try downloading it yourself from\n\n  ' .
      $url . '\n\n")';
  }
  echo($response);
} else {
  header("HTTP/1.0 404 Not Found");
  echo 'File not found. You might want to check that address. Seriously.';
  
  echo "\n\nSome debugging:<br/>" .
    'REQUEST_URL = ' . $request_url . '<br/>' .
    '$proper_request_stem = ' . $proper_request_stem . '<br/>';
}

?>