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

%typemap(in) PGconn * {
  $1 = (PGconn *) PyCapsule_GetPointer($input, "psycopg2.connection.native_connection");
}

%typemap(in) MYSQL * {
  $1 = (MYSQL *) PyCapsule_GetPointer($input, "_mysql.connection.native_connection");
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

