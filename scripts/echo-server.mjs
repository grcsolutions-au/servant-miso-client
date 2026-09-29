import { createServer } from 'node:http';

const port = Number(process.env.ECHO_PORT ?? 8081);
const corsHeaders = {
  'Access-Control-Allow-Origin': '*',
  'Access-Control-Allow-Methods': 'GET, POST, PUT, OPTIONS',
  'Access-Control-Allow-Headers': 'Content-Type, Accept, Authorization',
  'Access-Control-Max-Age': '86400',
};

const server = createServer((request, response) => {
  for (const [name, value] of Object.entries(corsHeaders)) {
    response.setHeader(name, value);
  }
  if (request.method === 'OPTIONS') {
    response.statusCode = 204;
    response.end();
    return;
  }

  const url = new URL(request.url ?? '/', `http://${request.headers.host ?? '127.0.0.1'}`);
  if (request.method === 'GET' && url.pathname === '/get') {
    response.setHeader('Content-Type', 'application/json');
    response.end(JSON.stringify({ url: url.href }));
    return;
  }

  const statusMatch = /^\/status\/(\d+)$/.exec(url.pathname);
  if (request.method === 'GET' && statusMatch) {
    const status = Number(statusMatch[1]);
    if (status >= 100 && status <= 599) {
      response.statusCode = status;
      response.end();
      return;
    }
  }

  response.statusCode = 404;
  response.end('Not Found');
});

server.on('error', error => {
  console.error(error);
  process.exitCode = 1;
});

server.listen(port, '127.0.0.1', () => {
  console.log(`echo test server listening on http://127.0.0.1:${port}`);
});