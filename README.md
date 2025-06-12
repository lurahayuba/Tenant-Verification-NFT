# 🏠 Tenant Verification NFT

A blockchain-based solution for verifying tenant reliability and rental history using NFTs.

## 🎯 Overview

The Tenant Verification NFT system allows landlords to create and maintain verified tenant profiles on the Stacks blockchain. Each profile is represented as an NFT containing:

- Rental duration
- Payment consistency score
- Conduct score
- Landlord references
- Verification status

## 💡 Features

- Create tenant profiles as NFTs
- Update tenant information
- Add landlord references
- Verify tenant profiles
- Query tenant history and scores

## 🚀 Usage

### For Landlords

1. Create a tenant profile:
```clarity
(contract-call? .tenant-verification-nft create-tenant-profile tenant-address)
```

2. Update tenant information:
```clarity
(contract-call? .tenant-verification-nft update-tenant-profile token-id rental-end monthly-rent payment-score conduct-score)
```

3. Add a reference:
```clarity
(contract-call? .tenant-verification-nft add-tenant-reference token-id "Excellent tenant, always paid on time")
```

### For Property Managers

1. Verify a tenant:
```clarity
(contract-call? .tenant-verification-nft verify-tenant token-id)
```

2. Check tenant profile:
```clarity
(contract-call? .tenant-verification-nft get-tenant-profile token-id)
```

## 🔒 Security

- Only contract owner can create new profiles
- Only the original landlord can update profiles and add references
- Verification status is permanent and cannot be modified once set

## 🤝 Contributing

Feel free to submit issues and enhancement requests!
```
