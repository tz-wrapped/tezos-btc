# SPDX-FileCopyrightText: 2019 Example
#
# SPDX-License-Identifier: LicenseRef-Proprietary
#

license_copyright_text = "SPDX-FileCopyrightText: 2019 Example"
license_identifier = "SPDX-License-Identifier: LicenseRef-Proprietary"

fout = open('out.txt', 'w')
fout.write(license_copyright_text)
fout.write('\n')
fout.write(license_identifier)
fout.close()
