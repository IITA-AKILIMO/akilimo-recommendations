# AKILIMO R Recommendation Engine

This guide covers the deployment and management of the AKILIMO R API as a systemd service on Linux systems.

## Status
![AKILIMO API](https://cronitor.io/badges/vAYGcx/production/ExvO8BeveiQL0YQDpvu_jVQk3oE/detailed.svg)

## Overview

The AKILIMO R API runs as a managed systemd service, providing automatic restarts, resource management, logging, and security hardening. This ensures reliable operation in production environments.

## Prerequisites

- Linux system with systemd (Ubuntu 16.04+, Debian 8+, CentOS 7+, etc.)
- R installed at `/usr/bin/Rscript`
- User `akilimo` with appropriate permissions
- Project files located at `/home/akilimo/projects/new_akilimo`

## Project Structure

```
~/projects/new_akilimo/
├── systemd/
│   ├── akilimo-api.service.example  # Template (tracked in Git)
│   └── akilimo-api.service          # Your config (gitignored)
├── R/
│   └── api2.R
└── logs/
```

**Important:** Add to your `.gitignore`:
```
systemd/akilimo-api.service
```

This ensures environment-specific configurations aren't committed to the repository.

## Installation

### 1. Create the Service File

The repository contains a template file `akilimo-api.service.example`. Copy and customize it for your environment:

```bash
# Copy the example file
cp ~/projects/new_akilimo/systemd/akilimo-api.service.example ~/projects/new_akilimo/systemd/akilimo-api.service

# Edit the file to match your environment (paths, ports, etc.)
nano ~/projects/new_akilimo/systemd/akilimo-api.service
```

**Note:** The `.example` file is tracked in Git, but `akilimo-api.service` is gitignored to prevent committing environment-specific configurations.

### 2. Create the Symbolic Link

Link the configured service file to systemd:

```bash
sudo ln -s ~/projects/new_akilimo/systemd/akilimo-api.service /etc/systemd/system/akilimo-api.service
```

**Why use a symlink?**
- Changes to the file in your project automatically apply to the service
- Service file stays with your project (not scattered in system directories)
- Easy to manage and update

**Verify the symlink:**
```bash
ls -la /etc/systemd/system/akilimo-api.service
```

You should see: `... -> /home/akilimo/projects/new_akilimo/systemd/akilimo-api.service`

### 2. Create Required Directories

Ensure log directories exist with proper permissions:

```bash
sudo mkdir -p /home/akilimo/projects/new_akilimo/logs
sudo chown -R akilimo:akilimo /home/akilimo/projects/new_akilimo/logs
```

### 3. Reload Systemd

Tell systemd to recognize the new service:

```bash
sudo systemctl daemon-reload
```

### 4. Enable the Service

Configure the service to start automatically on boot:

```bash
sudo systemctl enable akilimo-api.service
```

### 6. Start the Service

```bash
sudo systemctl start akilimo-api.service
```

## Beta Instance (`experimental` branch)

A parallel beta instance runs alongside production using a separate directory and port.

| | Production | Beta |
|---|---|---|
| Directory | `/home/akilimo/projects/new_akilimo` | `/home/akilimo/projects/akilimo-beta` |
| Service | `akilimo-api.service` | `akilimo-api-beta.service` |
| Port | 8000 | 8001 |
| Branch | `main` | `experimental` |

### 1. Create the Beta Service File

```bash
# Clone the repo into the beta directory (first time only)
git clone <repo-url> /home/akilimo/projects/akilimo-beta
cd /home/akilimo/projects/akilimo-beta
git checkout experimental

# Copy the example file
cp ~/projects/akilimo-beta/systemd/akilimo-api-beta.service.example ~/projects/akilimo-beta/systemd/akilimo-api-beta.service

# Edit the file to match your environment (paths, ports, etc.)
nano ~/projects/akilimo-beta/systemd/akilimo-api-beta.service
```

### 2. Create the Symbolic Link

```bash
sudo ln -s ~/projects/akilimo-beta/systemd/akilimo-api-beta.service /etc/systemd/system/akilimo-api-beta.service
```

**Verify the symlink:**
```bash
ls -la /etc/systemd/system/akilimo-api-beta.service
```

### 2. Create Required Directories

```bash
sudo mkdir -p /home/akilimo/projects/akilimo-beta/logs
sudo chown -R akilimo:akilimo /home/akilimo/projects/akilimo-beta/logs
```

### 3. Reload Systemd

```bash
sudo systemctl daemon-reload
```

### 4. Enable the Service

```bash
sudo systemctl enable akilimo-api-beta.service
```

### 5. Start the Service

```bash
sudo systemctl start akilimo-api-beta.service
```

## Service Management

### Check Service Status

```bash
# Production
sudo systemctl status akilimo-api.service

# Beta
sudo systemctl status akilimo-api-beta.service
```

### Start the Service

```bash
sudo systemctl start akilimo-api.service
sudo systemctl start akilimo-api-beta.service
```

### Stop the Service

```bash
sudo systemctl stop akilimo-api.service
sudo systemctl stop akilimo-api-beta.service
```

### Restart the Service

```bash
sudo systemctl restart akilimo-api.service
sudo systemctl restart akilimo-api-beta.service
```

### Reload Configuration

If you modify the service file in your project:

```bash
# The symlink automatically reflects changes, just reload systemd
sudo systemctl daemon-reload
sudo systemctl restart akilimo-api.service
sudo systemctl restart akilimo-api-beta.service
```

### Disable Auto-start

```bash
sudo systemctl disable akilimo-api.service
sudo systemctl disable akilimo-api-beta.service
```

## Monitoring and Logs

### View Real-time Logs

```bash
# Production
sudo journalctl -u akilimo-api.service -f

# Beta
sudo journalctl -u akilimo-api-beta.service -f
```

### View Recent Logs

```bash
sudo journalctl -u akilimo-api.service -n 100
sudo journalctl -u akilimo-api-beta.service -n 100
```

### View Logs Since Boot

```bash
sudo journalctl -u akilimo-api.service -b
sudo journalctl -u akilimo-api-beta.service -b
```

### View Logs for Specific Time Period

```bash
sudo journalctl -u akilimo-api.service --since "2024-01-01" --until "2024-01-02"
sudo journalctl -u akilimo-api-beta.service --since "2024-01-01" --until "2024-01-02"
```

### Filter by Priority

```bash
# Show only errors
sudo journalctl -u akilimo-api.service -p err
sudo journalctl -u akilimo-api-beta.service -p err

# Show warnings and above
sudo journalctl -u akilimo-api.service -p warning
sudo journalctl -u akilimo-api-beta.service -p warning
```

## Configuration

### Resource Limits

The service includes default resource limits. Adjust these in the service file based on your needs:

```ini
MemoryMax=2G        # Maximum memory usage
CPUQuota=200%       # CPU limit (200% = 2 cores)
LimitNOFILE=65536   # Maximum open files
```

### Restart Behavior

The service automatically restarts on failure with rate limiting:

- **Restart Policy**: Only on failure (not on clean exit)
- **Restart Delay**: 5 seconds between attempts
- **Rate Limit**: Maximum 5 restarts within 5 minutes

To change restart behavior, modify these lines:

```ini
Restart=on-failure          # Options: no, always, on-success, on-failure, on-abnormal, on-abort, on-watchdog
RestartSec=5s
StartLimitInterval=300s
StartLimitBurst=5
```

### Environment Variables

Add additional environment variables in the `[Service]` section:

```ini
Environment=API_PORT=8080
Environment=LOG_LEVEL=INFO
Environment=R_LIBS_USER=/home/akilimo/R/library
```

### File Permissions

The service includes security hardening. If your API needs to write to additional directories, add them to `ReadWritePaths`:

```ini
ReadWritePaths=/home/akilimo/projects/new_akilimo/logs /home/akilimo/data /tmp
```

## Troubleshooting

### Service Won't Start

1. Check the service status:
   ```bash
   sudo systemctl status akilimo-api.service
   ```

2. View detailed logs:
   ```bash
   sudo journalctl -u akilimo-api.service -n 50
   ```

3. Verify file permissions:
   ```bash
   ls -la /home/akilimo/projects/new_akilimo/R/api2.R
   ```

4. Test R script manually:
   ```bash
   sudo -u akilimo /usr/bin/Rscript /home/akilimo/projects/new_akilimo/R/api2.R
   ```

### Service Keeps Restarting

Check if the service is hitting the restart rate limit:

```bash
sudo systemctl status akilimo-api.service
```

Look for "Start request repeated too quickly" messages. Review logs for the actual error causing the restarts.

### Permission Denied Errors

Ensure the `akilimo` user owns the necessary files:

```bash
sudo chown -R akilimo:akilimo /home/akilimo/projects/new_akilimo
```

### Memory or CPU Issues

Check resource usage:

```bash
systemctl show akilimo-api.service -p MemoryCurrent -p CPUUsageNSec
```

Adjust limits in the service file if needed.

### Port Already in Use

If the API fails to bind to a port:

```bash
sudo netstat -tlnp | grep <port_number>
# or
sudo ss -tlnp | grep <port_number>
```

Kill the process using the port or configure your API to use a different port.

## Security Features

The service includes several security hardening measures:

- **NoNewPrivileges**: Prevents privilege escalation
- **PrivateTmp**: Isolates /tmp directory
- **ProtectSystem**: Makes system directories read-only
- **ProtectHome**: Restricts access to home directories
- **ProtectKernelTunables**: Prevents kernel parameter changes
- **UMask**: Sets secure file creation permissions

These settings may need adjustment if your application requires broader system access.

## Performance Tuning

### For High-Traffic APIs

Increase resource limits:

```ini
MemoryMax=4G
CPUQuota=400%
LimitNOFILE=131072
```

### For Low-Resource Environments

Reduce limits:

```ini
MemoryMax=512M
CPUQuota=100%
```

### Adjust Start Timeout

If your R API takes longer to initialize:

```ini
TimeoutStartSec=120s
```

## Backup and Maintenance

### Before Updates

1. Stop the service:
   ```bash
   sudo systemctl stop akilimo-api.service
   ```

2. Backup the current version:
   ```bash
   cp -r /home/akilimo/projects/new_akilimo /home/akilimo/backups/new_akilimo_$(date +%Y%m%d)
   ```

3. Deploy updates and restart:
   ```bash
   sudo systemctl start akilimo-api.service
   ```

### Service File Backup

Keep a copy of your service configuration:

```bash
sudo cp /etc/systemd/system/akilimo-api.service /home/akilimo/backups/
```

## Support

For issues or questions:

- Check logs: `sudo journalctl -u akilimo-api.service -f`
- Review service status: `sudo systemctl status akilimo-api.service`
- Consult the R API documentation
- Report issues to your development team

## License

This project is licensed under the MIT License.

```
MIT License

Copyright (c) 2024 Akilimo Project

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

## Documentation

| Document | Description |
|----------|-------------|
| [docs/EXPERIMENTAL-REVIEW.md](docs/EXPERIMENTAL-REVIEW.md) | Comprehensive review of all changes in the `experimental` branch vs `main` — used for team review before merging |

## Contributors

- [@rhijmans](https://github.com/rhijmans)
- [@omilika](https://github.com/omilika)
- [@masgeek](https://github.com/masgeek)

We welcome contributions! Please feel free to submit issues and pull requests.
