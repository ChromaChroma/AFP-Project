#!/bin/bash
npx http-server -p 8000 --cors='*' --proxy http://localhost:8000/
