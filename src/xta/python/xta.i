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

%ignore  PostgresqlXaResource(PGconn *connection, std::string const& name,
                              std::string const& open_info);
%include "PostgresqlXaResource.hpp"
%include "Xid.hpp"
%include "Transaction.hpp"
%include "TransactionManager.hpp"

