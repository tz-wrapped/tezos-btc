# SPDX-FileCopyrightText: 2019 Example
#
# SPDX-License-Identifier: LicenseRef-Proprietary
#

license =
"- SPDX-FileCopyrightText: 2019 Example\n" +
"- SPDX-License-Identifier: LicenseRef-Proprietary"

fout = open('out.txt', 'w')
fout.write(license)
fout.close()
