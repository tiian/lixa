%module xta
%{
#define SWIG_FILE_WITH_INIT
#include "Xta.hpp"
#include "XaResource.hpp"
#include "NativeXaResource.hpp"
#include "AcquiredXaResource.hpp"
#include "MysqlXaResource.hpp"
#include "PostgresqlXaResource.hpp"
#include "Xid.hpp"
#include "Transaction.hpp"
#include "TransactionManager.hpp"

typedef struct {
        MYSQL   *mysql;
        int     other_stuff_we_dont_care_of;
} MY_MYSQL;

/*
#include "/usr/local/include/php/ext/mysqli/php_mysqli_structs.h"
*/
%}

%include exception.i
%include std_string.i

%exception {
        try {
                $action
        } catch (xta::Exception e) {
                string text = "XTA exception in function " +
                    e.where() + " [" + e.getReturnCodeText() + "]";
                SWIG_exception(SWIG_RuntimeError, text.c_str());
        }
}

/*
%typemap(in) PGconn * {
  $1 = (PGconn *) PyCapsule_GetPointer($input($input), "psycopg2.connection.native_connection");
}
*/

%typemap(in) MYSQL * {
/*
  $1 = Z_OBJ_P(&($input($input)));
*/

zval *zv_ptr = &($input);
MY_MYSQL *my_mysql;

switch (Z_TYPE_P(zv_ptr)) {
        case IS_NULL:
            php_printf("NULL: null\n");
            break;
        case IS_TRUE:
            php_printf("BOOL: true\n");
            break;
        case IS_FALSE:
            php_printf("BOOL: false\n");
            break;
        case IS_LONG:
            php_printf("LONG: %ld\n", Z_LVAL_P(zv_ptr));
            break;
        case IS_DOUBLE:
            php_printf("DOUBLE: %g\n", Z_DVAL_P(zv_ptr));
            break;
        case IS_STRING:
            php_printf("STRING: value=\"");
            PHPWRITE(Z_STRVAL_P(zv_ptr), Z_STRLEN_P(zv_ptr));
            php_printf("\", length=%zd\n", Z_STRLEN_P(zv_ptr));
            break;
        case IS_RESOURCE:
            php_printf("RESOURCE: id=%d\n", Z_RES_HANDLE_P(zv_ptr));
            break;
        case IS_ARRAY:
            php_printf("ARRAY: hashtable=%p\n", Z_ARRVAL_P(zv_ptr));
            break;
        case IS_OBJECT:
            php_printf("OBJECT: object=%p\n", Z_OBJ_P(zv_ptr));
            php_printf("OBJECT content=%p\n", Z_OBJ_P(zv_ptr)->ce);
            break;
    }
  $1 = ((MY_MYSQL *) Z_OBJ_P(zv_ptr))->mysql;
/*
  $1 = ((MY_MYSQL *)Z_OBJ_P(&($input))->ce)->mysql;
*/
}

%include "Xta.hpp"
%include "XaResource.hpp"
%include "NativeXaResource.hpp"
%include "AcquiredXaResource.hpp"
%include "PostgresqlXaResource.hpp"
%include "MysqlXaResource.hpp"
%include "Xid.hpp"
%include "Transaction.hpp"
%include "TransactionManager.hpp"

