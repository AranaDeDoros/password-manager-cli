# Password Manager

A simple CLI password manager built with Scala 3. Stores encrypted entries locally and allows temporary clipboard access.

## Stack
- **Java 24**
- **Scala**: 3.3.7
- **Libraries**:
    - io.circe: circe-core, circe-generic, circe-parser (0.14.15)
    - de.mkammerer: argon2-jvm (2.12)

## Setup

1. **Set the application secret** (used for encryption):

```bash
export APP_SECRET=$(openssl rand -base64 32)
```
2. **Run the app to initialize the database and set up master password:**
```bash
sbt run --init
```

## Usage
```bash
# Add an entry
sbt "run --add example.com mykey"

# Delete an entry
sbt "run --del example.com mykey"

# List all entries
sbt "run --list"

# Search for an entry (password copied temporarily to clipboard)
sbt "run --search mykey"

# Display help
sbt "run --help"

```
*When searching for a password, it will be copied to the clipboard for 10 seconds for security.
Always keep your **APP_SECRET safe**; losing it will make your database unrecoverable.*

## TODO
- Allow cancellation of password copying.
- Refactor Error handling with better types
- Make IO more pure.