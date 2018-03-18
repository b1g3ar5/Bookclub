

<?php


    function setAvailable($match, $yesNo)
    {
        $name = getVar("name");
        $password = GetVar("password");
        //setDatabase($name, $match, $yesNo);
        if($yesNo==1)
            $availability = "AVAILABLE";
        else
            $availability = "NOT AVAILABLE";
        //$ret = '<a href="' . headUrl() . '?name=' . $name . '&password=' . $password . '">Set to ' . $availability . '</a>';
        $ret = '<br/><a href="' . headUrl() . '?name=' . $name . '&password=' . $password . '">Set to ' . $availability . '</a><br/>';
        return $ret;
    }

    function setDatabase($name, $match, $yesNo)
    {
        $pid = getPlayerId($name);
        $mid = getMatchId($match);
        $avty = csvToArray('../csv/availability.csv');
        $avty[$pid][$mid] = $yesNo;
        arrayToCsv('../csv/availability1.csv', $avty);
    }

    function getPlayerId($name)
    {
        $ps = csvToArray('../csv/players.csv', ',');
        for($i=0;i<count($ps);$i++)
        {
            if($ps[$i]["Name"]==$name)
                return $i;;
        }
        return -1;
    }

    function getMatchId($name)
    {
        $ps = csvToArray('../csv/matches.csv', ',');
        for($i=0;i<count($ps);$i++)
        {
            if($ps[$i]["Team"]==$name)
                return $i;;
        }
        return -1;
    }




    function printArray($a)
    {
        $ret = '';
        $ret = $ret . '<pre>';
        $ret = $ret . var_dump($a);
        $ret = $ret . '</pre>';
        return $ret;
    }

    function fullUrl()
    {
        $s = empty($_SERVER["HTTPS"]) ? '' : ($_SERVER["HTTPS"] == "on") ? "s" : "";
        $sp = strtolower($_SERVER["SERVER_PROTOCOL"]);
        $protocol = substr($sp, 0, strpos($sp, "/")) . $s;
        $port = ($_SERVER["SERVER_PORT"] == "80") ? "" : (":".$_SERVER["SERVER_PORT"]);
        return $protocol . "://" . $_SERVER['SERVER_NAME'] . $port . $_SERVER['REQUEST_URI'];
    }

    function headUrl()
    {
        $s = empty($_SERVER["HTTPS"]) ? '' : ($_SERVER["HTTPS"] == "on") ? "s" : "";
        $protocol = substr(strtolower($_SERVER["SERVER_PROTOCOL"]), 0, strpos(strtolower($_SERVER["SERVER_PROTOCOL"]), "/")) . $s;
        $port = ($_SERVER["SERVER_PORT"] == "80") ? "" : (":".$_SERVER["SERVER_PORT"]);
        $uri = $protocol . "://" . $_SERVER['SERVER_NAME'] . $port . $_SERVER['REQUEST_URI'];
        $segments = explode('?', $uri, 2);
        $url = $segments[0];
        return $url;
    }


    function getCsvFile($csvFileName='', $csvNumColumns)
    {
        $csvData = file_get_contents($csvFileName); 
        $csvDelim = ","; 
        $data = array_chunk(str_getcsv($csvData, $csvDelim), $csvNumColumns); 
        return $data;
    }

    
    function csvToArray($filename='', $delimiter=',')
    {
        if(!file_exists($filename) || !is_readable($filename))
            return FALSE;

        $header = NULL;
        $data = array();
        if (($handle = fopen($filename, 'r')) !== FALSE)
        {
            while (($row = fgetcsv($handle, 1000, $delimiter)) !== FALSE)
            {
                if(!$header)
                    $header = $row;
                else
                    $data[] = array_combine($header, $row);
            }
            fclose($handle);
        }
        return $data;
    }    
    

    function csvToTable($fname)
    {
        $fp = fopen('../csv/' . $fname . '.csv','r') or die("can't open file");
        print "<table>\n";
        while($csv_line = fgetcsv($fp,1024)) {
            print '<tr>';
            for ($i = 0, $j = count($csv_line); $i < $j; $i++) {
                print '<td>'.$csv_line[$i].'</td>';
            }
            print "</tr>\n";
        }
        print '</table>';
        fclose($fp) or die("can't close file");
    }

    
    
    
    
    function getVar($vname)
    {
        return htmlspecialchars($_GET[$vname]);
    }
        

    function getEdit($match, $code){
        if(getVar("password")==$code){
            return setAvailable($match, 1) . " " . setAvailable($match, 0) ;
        } else {
            return "Not editable";
        }
    }
?>
