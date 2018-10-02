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

%include "Xta.hpp"
%include "XaResource.hpp"
%include "NativeXaResource.hpp"
%include "AcquiredXaResource.hpp"
%include "MysqlXaResource.hpp"

%typemap(in) PGconn * {
  $1 = (PGconn *) PyCapsule_GetPointer($input, "psycopg2.connection._raw_pgconn");
}

%ignore  PostgresqlXaResource(unsigned long long, char const*, char const*);
%ignore  PostgresqlXaResource(PGconn *,std::string const &,std::string const &);

%include "PostgresqlXaResource.hpp"
%include "Xid.hpp"
%include "Transaction.hpp"
%include "TransactionManager.hpp"

