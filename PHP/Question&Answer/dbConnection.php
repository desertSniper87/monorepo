
<html>
   <head>
      <title>Connect to MariaDB Server</title>
   </head>

   <body>
      <?php
      /*
       *   $dbhost = 'localhost:8001';
       *   $dbuser = 'torsho';
       *   $dbpass = '1234';
       *   $conn = mysql_connect($dbhost, $dbuser, $dbpass);
       *
       *   if(! $conn ) {
       *      die('Could not connect: ' . mysql_error());
       *   }
       *   
       *   echo 'Connected successfully';
       *   //mysql_close($conn);
       */
      ?>

      <?php
          $con= new mysqli('localhost','torsho','1234','quiz')or die("Could not connect to mysql".mysqli_error($con));
      ?>
   </body>
</html>
