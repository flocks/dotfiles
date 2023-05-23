#!/usr/bin/env bash

ARN="arn:aws:iam::364737596256:mfa/florent.teissier"

if [[ $# != 1 ]]; then
    echo "need token" >&2
    exit 1
fi
token=$1

aws sts get-session-token --serial-number $ARN --token-code $token --profile default | jq -r '.Credentials | ["AWS_ACCESS_KEY_ID=\(.AccessKeyId)", "AWS_SECRET_ACCESS_KEY=\(.SecretAccessKey)", "AWS_SESSION_TOKEN=\(.SessionToken)"] | "export " + .[]'
