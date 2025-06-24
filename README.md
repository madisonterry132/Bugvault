
# 🐛 Bugvault - Smart Contract Bug Bounty Board

A decentralized bug bounty platform built on Stacks blockchain where security researchers can find and report vulnerabilities in exchange for STX rewards.

## 🚀 Features

- **Create Bug Bounties**: Post bounties with STX rewards for finding security vulnerabilities
- **Submit Bug Reports**: Security researchers can submit detailed vulnerability reports
- **Reward Distribution**: Automatic STX distribution when submissions are approved
- **Deadline Management**: Time-bound bounties with automatic expiration handling
- **Fund Security**: Bounty funds are held in escrow until completion or cancellation

## 📋 Contract Functions

### Public Functions

#### `create-bounty`
Creates a new bug bounty with STX reward locked in escrow.
```clarity
(create-bounty title description reward duration)
```

#### `submit-bug-report`
Submit a vulnerability report for an active bounty.
```clarity
(submit-bug-report bounty-id description)
```

#### `approve-submission`
Approve a submission and transfer reward to the researcher (bounty creator only).
```clarity
(approve-submission bounty-id submission-id)
```

#### `reject-submission`
Reject a submission (bounty creator only).
```clarity
(reject-submission bounty-id submission-id)
```

#### `cancel-bounty`
Cancel an active bounty and refund the creator.
```clarity
(cancel-bounty bounty-id)
```

#### `claim-expired-bounty`
Reclaim funds from an expired bounty with no approved submissions.
```clarity
(claim-expired-bounty bounty-id)
```

### Read-Only Functions

#### `get-bounty`
Retrieve bounty details by ID.

#### `get-submission`
Retrieve submission details by ID.

#### `get-user-submission`
Get a user's submission for a specific bounty.

#### `is-bounty-active`
Check if a bounty is currently active and not expired.

## 🛠️ Usage Examples

### Creating a Bounty
```bash
clarinet console
```
```clarity
(contract-call? .Bugvault create-bounty "Smart Contract Audit" "Find vulnerabilities in our DeFi protocol" u1000000 u1000)
```

### Submitting a Bug Report
```clarity
(contract-call? .Bugvault submit-bug-report u1 "Found reentrancy vulnerability in withdraw function")
```

### Approving a Submission
```clarity
(contract-call? .Bugvault approve-submission u1 u1)
```

## 🔒 Security Features

- **Escrow System**: Funds are locked in contract until bounty completion
- **Access Control**: Only bounty creators can approve/reject submissions
- **Deadline Enforcement**: Automatic expiration handling
- **Single Submission**: One submission per user per bounty
- **Fund Recovery**: Creators can reclaim funds from expired bounties

## 🧪 Testing

```bash
clarinet test
```

## 📦 Deployment

```bash
clarinet deploy --testnet
```

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## 📄 License

MIT License - feel free to use this contract for your bug bounty platform!

---



