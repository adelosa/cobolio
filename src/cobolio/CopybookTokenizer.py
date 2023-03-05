import re


class CopybookTokenizer:

    def __init__(self, copybook_data):

        self.line_sep = r'[.,;]?$|[.,;]?\s'
        self.s_quote = r"'[^']*'"
        self.d_quote = r'"[^"]*"'
        self.reg_ex = re.compile("(%s|%s|%s)" % (self.line_sep, self.s_quote, self.d_quote))

        self.tokens = self.tokenize(copybook_data)

    def clean(self, copybook_data):

        copybook_data = [line[6:72].rstrip() for line in copybook_data.split('\n')
                         if len(line) > 6 and line[6] not in ('*', '/')]
        copybook_data = [line for line in copybook_data if line.strip() not in ("EJECT", "SKIP1", "SKIP2", "SKIP3")]
        copybook_data = [line for line in copybook_data if len(line) > 0]
        copybook_data = ' '.join(copybook_data)

        return copybook_data

    def tokenize(self, copybook_data):

        clean_copybook_data = self.clean(copybook_data)
        tokens = [token.strip() for token in re.split(self.reg_ex, clean_copybook_data) if token.strip()]

        return tokens

    def get_token(self):

        if self.tokens:
            token = self.tokens.pop(0)
        else:
            token = None

        return token

    def put_token(self, token):

        if token:
            self.tokens.insert(0, token)
