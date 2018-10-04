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
%exception {
        try {
                $action
        } catch (xta::Exception e) {
                string text = "XTA exception [" + e.getReturnCodeText() + "]";
                SWIG_exception(SWIG_RuntimeError, text.c_str());
        }
}

%include "Xta.hpp"
%include "XaResource.hpp"
%include "NativeXaResource.hpp"
%include "AcquiredXaResource.hpp"
%include "MysqlXaResource.hpp"

%typemap(in) PGconn * {
  $1 = (PGconn *) PyCapsule_GetPointer($input, "psycopg2.connection._raw_pgconn");
}

%ignore  PostgresqlXaResource(PGconn *,std::string const &,std::string const &);

%include "PostgresqlXaResource.hpp"
%include "Xid.hpp"
%include "Transaction.hpp"
%include "TransactionManager.hpp"

