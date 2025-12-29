# hledger-api

A JSON API server for [hledger](https://hledger.org) journal files, providing programmatic access to your plain-text accounting data.

## Features

- **Accounts API** - List accounts, get account details, balances, and register (transaction history)
- **Transactions API** - Query and filter transactions with pagination
- **Reports API** - Balance sheet, income statement, and cash flow reports
- **Meta API** - Version info, commodities, payees, and tags
- **OpenAPI 3.0** - Auto-generated API documentation at `/openapi.json`

## Requirements

### Local Build
- GHC 9.12.2
- hledger-lib >= 1.43
- Stack

## Quick Start

### With Docker (Recommended)

```bash
docker run -d -p 8080:8080 \
  -v $(pwd)/example.journal:/data/ledger.journal:ro \
  ghcr.io/wolfsblu/hledger-api:latest
```

### Local Build

```bash
stack build

# With command line args
stack run -- --journal /path/to/your.journal --port 8080

# Or with LEDGER_FILE environment variable
export LEDGER_FILE=~/.hledger.journal
stack run
```

## API Endpoints

### Accounts
- `GET /api/v1/accounts` - List all accounts
- `GET /api/v1/accounts/:name` - Get account details
- `GET /api/v1/accounts/:name/balance` - Get account balance history
- `GET /api/v1/accounts/:name/register` - Get account register

### Transactions
- `GET /api/v1/transactions` - List transactions (with filtering & pagination)
- `GET /api/v1/transactions/:index` - Get transaction by index

### Reports
- `GET /api/v1/reports/balance-sheet` - Balance sheet report
- `GET /api/v1/reports/income-statement` - Income statement report
- `GET /api/v1/reports/cash-flow` - Cash flow report

### Meta
- `GET /api/v1/version` - API and hledger version info
- `GET /api/v1/commodities` - List of commodities used
- `GET /api/v1/payees` - List of payees/descriptions
- `GET /api/v1/tags` - List of tags

### Documentation
- `GET /openapi.json` - OpenAPI 3.0 specification
