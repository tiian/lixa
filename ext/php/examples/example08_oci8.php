<?php

$conn = oci_connect('hr', 'hr', 'localhost/XE');
if (!$conn) {
    $e = oci_error();
    trigger_error(htmlentities($e['message'], ENT_QUOTES), E_USER_ERROR);
}

// Prepare the statement
$stid = oci_parse($conn, "SELECT COUNTRY_ID, COUNTRY_NAME FROM countries WHERE COUNTRY_ID=:ci");
if (!$stid) {
    $e = oci_error($conn);
    trigger_error(htmlentities($e['message'], ENT_QUOTES), E_USER_ERROR);
}
$ci = "RS";
oci_bind_by_name($stid, ':ci', $ci);

// Perform the logic of the query
$r = oci_execute($stid);
if (!$r) {
    $e = oci_error($stid);
    trigger_error(htmlentities($e['message'], ENT_QUOTES), E_USER_ERROR);
}

// Fetch the results of the query
echo "Result set:\n";
while ($row = oci_fetch_array($stid, OCI_ASSOC)) {
	echo "country_id=" . $row['COUNTRY_ID'] . ", country_name=" . $row['COUNTRY_NAME'] . "\n";
}

oci_free_statement($stid);
oci_close($conn);

?>

